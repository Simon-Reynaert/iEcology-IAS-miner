import pandas as pd
import time
import traceback
import os
from pygbif import occurrences
from datetime import datetime, timedelta

# Load species list from CSV
species_df = pd.read_csv("list_of_union_concern.csv")
species_list = species_df["Scientific Name"].dropna().unique().tolist()

# Expanded list of European countries
european_countries = [
    "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR", "HU",
    "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK", "SI", "ES", "SE",
    "IS", "LI", "NO", "CH", "GB", "AL", "BA", "ME", "MK", "RS", "XK"
]

# Define the date range
start_date = "2022-01-01"
end_date = pd.Timestamp.today().strftime("%Y-%m-%d")

# Filter cultivated records?
filter_wild = False

# Output file path
output_file = "GBIF_species_occurrences_EU.csv"
header_written = os.path.exists(output_file)

# Function to fetch and save data for a given date range
def fetch_and_save_data(species, country, start_date, end_date, output_file, header_written):
    event_date_range = f"{start_date},{end_date}"
    offset = 0
    limit = 300
    country_records = []
    total_records = None
    fetched_records = 0

    while True:
        retries = 0
        max_retries = 2
        success = False

        while retries <= max_retries:
            try:
                response = occurrences.search(
                    scientificName=species,
                    country=country,
                    eventDate=event_date_range,
                    limit=limit,
                    offset=offset
                )
                time.sleep(1)  # Pause between requests
                success = True
                break
            except Exception as e:
                retries += 1
                print(f"⚠️ Retry {retries}/{max_retries} failed for {species} in {country} at offset {offset}, date range: {event_date_range}")
                traceback.print_exc()
                time.sleep(2)

        if not success:
            print(f"❌ Skipping {species} in {country} at offset {offset} after {max_retries} retries, date range: {event_date_range}.\n")
            break

        if total_records is None:
            total_records = response.get("count", 0)
            print(f"📊 Total available records for date range {event_date_range}: {total_records}")

        records = response.get("results", [])
        if not records:
            break  # No more data

        for rec in records:
            establishment = rec.get("establishmentMeans", "")
            if filter_wild and establishment and "cultivated" in establishment.lower():
                continue

            country_records.append({
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

        fetched_records += len(records)
        print(f"📥 Fetched {fetched_records}/{total_records} records for date range {event_date_range}...", end="\r")

        offset += limit  # Move to next page

    if country_records:
        df = pd.DataFrame(country_records)
        df.to_csv(output_file, mode='a', index=False, header=not header_written)
        header_written = True
        print(f"\n✅ Saved {len(df)} records for {species} in {country}, date range: {event_date_range}")
    else:
        print(f"ℹ️ No records found for {species} in {country}, date range: {event_date_range}")
    return header_written

# Loop through each species and country
for species in species_list:
    for country in european_countries:
        print(f"\n🔍 Fetching {species} in {country}...")

        # Initial check to estimate record count
        initial_response = occurrences.search(scientificName=species, country=country, eventDate=f"{start_date},{end_date}", limit=1)
        total_estimated_records = initial_response.get("count", 0)

        if total_estimated_records > 10000:
            # Fetch by month
            current_date = datetime.strptime(start_date, "%Y-%m-%d")
            end_date_dt = datetime.strptime(end_date, "%Y-%m-%d")

            while current_date <= end_date_dt:
                next_month = current_date + timedelta(days=32)  # Add a little extra to ensure next month
                next_month = next_month.replace(day=1)
                month_end = min(next_month - timedelta(days=1), end_date_dt)

                current_start_str = current_date.strftime("%Y-%m-%d")
                current_end_str = month_end.strftime("%Y-%m-%d")

                header_written = fetch_and_save_data(species, country, current_start_str, current_end_str, output_file, header_written)
                current_date = next_month
        else:
            # Fetch in one go
            header_written = fetch_and_save_data(species, country, start_date, end_date, output_file, header_written)