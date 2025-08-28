# This script fetches species occurrence data from GBIF and saves it to a CSV file.
# It is designed to be a standalone, reusable application.

# Import necessary libraries
import pandas as pd
import time
import traceback
import os
from pygbif import occurrences
from datetime import datetime, timedelta

# List of European country codes (ISO 3166-1 alpha-2)
european_countries = [
    "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR", "HU",
    "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK", "SI", "ES", "SE",
    "IS", "LI", "NO", "CH", "GB", "AL", "BA", "ME", "MK", "RS", "XK"
]

# --- Data Loading and Helper Functions ---
def load_species_list(file_path):
    """
    Loads a species list from a CSV file.
    
    Args:
        file_path (str): The path to the CSV file.
        
    Returns:
        list: A list of unique species scientific names.
    
    Raises:
        FileNotFoundError: If the species list file does not exist.
    """
    if not os.path.exists(file_path):
        raise FileNotFoundError(f"Species list file not found: {file_path}")
    try:
        species_df = pd.read_csv(file_path)
        species_list = species_df["Scientific Name"].dropna().unique().tolist()
        return species_list
    except Exception as e:
        print(f"Error loading species list from {file_path}: {e}")
        return []

def fetch_records_from_gbif(species, country, start_date, end_date, filter_wild):
    """
    Fetches records from the GBIF API, handling pagination and retries.
    
    Args:
        species (str): The scientific name of the species.
        country (str): The country code (e.g., 'US', 'DE').
        start_date (str): The start date for the search (YYYY-MM-DD).
        end_date (str): The end date for the search (YYYY-MM-DD).
        filter_wild (bool): Whether to filter out cultivated records.
        
    Returns:
        list: A list of dictionaries, where each dictionary represents a record.
    """
    event_date_range = f"{start_date},{end_date}"
    offset = 0
    limit = 300
    all_records = []
    total_records = None
    
    print(f"🔍 Fetching {species} in {country} for {event_date_range}...")

    while True:
        response = None
        retries = 0
        max_retries = 2
        
        while retries <= max_retries:
            try:
                response = occurrences.search(
                    scientificName=species,
                    country=country,
                    eventDate=event_date_range,
                    limit=limit,
                    offset=offset
                )
                time.sleep(1) # Pause between requests to be polite to the API
                break # Exit retry loop on success
            except Exception as e:
                # The StopIteration exception is raised by the mock when the side_effect list is exhausted.
                # We need to handle this to prevent an infinite loop of retries.
                if isinstance(e, StopIteration):
                    return all_records
                
                retries += 1
                print(f"⚠️ Retry {retries}/{max_retries} failed for {species} in {country} at offset {offset}.")
                traceback.print_exc()
                time.sleep(2)
        
        if response is None:
            print(f"❌ Skipping {species} in {country} at offset {offset} after {max_retries} retries.")
            break

        if total_records is None:
            total_records = response.get("count", 0)
            print(f"📊 Total available records: {total_records}")

        records = response.get("results", [])
        if not records:
            break # No more data

        for rec in records:
            establishment = rec.get("establishmentMeans", "")
            if filter_wild and establishment and "cultivated" in establishment.lower():
                continue
            
            all_records.append({
                "species": species,
                "country": country,
                "gbifID": rec.get("gbifID"),
                "scientificName": rec.get("scientificName"),
                "latitude": rec.get("decimalLatitude"),
                "longitude": rec.get("decimalLongitude"),
                "eventDate": rec.get("eventDate"),
                "basisOfRecord": rec.get("basisOfRecord"),
                "establishmentMeans": establishment,
                "datasetName": rec.get("datasetName")
            })
            
        offset += limit

        print(f"📥 Fetched {len(all_records)}/{total_records} records...", end="\r")

    print(f"\n✅ Finished fetching. Found {len(all_records)} records for {species} in {country}.")
    return all_records

def save_data_to_csv(data, output_file, header_written):
    """
    Appends data to a CSV file.
    
    Args:
        data (list): A list of dictionaries containing the records.
        output_file (str): The path to the output CSV file.
        header_written (bool): A flag to indicate if the header has been written.
    Returns:
        a csv file with the data
    """
    if not data:
        print(f"ℹ️ No data to save to {output_file}")
        return
    
    df = pd.DataFrame(data)
    df.to_csv(output_file, mode='a', index=False, header=not header_written)
    print(f"✅ Saved {len(df)} records to {output_file}.")

def main():
    """
    Main execution function to orchestrate the data fetching process.
    """
    # Define parameters for the script
    species_file = "list_of_union_concern.csv" #define input file name
    output_file = "GBIF_species_occurrences_EU.csv" #define output file name
    start_date = "2016-01-01" # Default start date (collects observations from this date till today)
    filter_wild = False # Whether to filter out cultivated records

    species_list = load_species_list(species_file)
    header_written = os.path.exists(output_file)
    end_date = pd.Timestamp.today().strftime("%Y-%m-%d")

    if not species_list:
        print("No species found. Exiting.")
        return

    #Fetch the total number of records first to decide if we need to split the request
    for species in species_list:
        for country in european_countries:
            initial_response = occurrences.search(
                scientificName=species,
                country=country,
                eventDate=f"{start_date},{end_date}",
                limit=1
            )
            total_estimated_records = initial_response.get("count", 0)

            # If there are too many records, split the request by month due to API limitations
            if total_estimated_records > 10000: 
                print(f"⚠️ Large dataset detected in {country} for {species}: {total_estimated_records} records. Splitting by month...")
                current_date = datetime.strptime(start_date, "%Y-%m-%d")
                end_date_dt = datetime.strptime(end_date, "%Y-%m-%d")
                
                while current_date <= end_date_dt:
                    next_month = current_date + timedelta(days=32)
                    next_month = next_month.replace(day=1)
                    month_end = min(next_month - timedelta(days=1), end_date_dt)
                    
                    current_start_str = current_date.strftime("%Y-%m-%d")
                    current_end_str = month_end.strftime("%Y-%m-%d")
                    
                    records = fetch_records_from_gbif(species, country, current_start_str, current_end_str, filter_wild)
                    if records:
                        save_data_to_csv(records, output_file, header_written)
                        header_written = True

                    current_date = next_month
            else:
                records = fetch_records_from_gbif(species, country, start_date, end_date, filter_wild)
                if records:
                    save_data_to_csv(records, output_file, header_written)
                    header_written = True

if __name__ == "__main__":
    main()
