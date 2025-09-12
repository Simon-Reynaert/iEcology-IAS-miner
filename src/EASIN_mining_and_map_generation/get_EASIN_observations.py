# =====================================================================================================
# EASIN Species Occurrence Data Fetcher
# =====================================================================================================
# 
# This script fetches invasive species occurrence data from the EASIN API for all EU countries.
# 
# Key Features:
# - Fetches data for 85 species across 28 EU countries
# - Handles API pagination automatically 
# - Implements robust retry logic for network failures
# - Saves data incrementally (can resume if interrupted)
# - Extracts coordinates from GeoJSON/WKT formats
# - Beautiful progress tracking with tqdm
# - Comprehensive error handling and logging
#
# API Structure:
# - Each species has an EASIN ID (e.g., R00212)
# - Data is fetched country-by-country to avoid timeouts
# - Coordinates come in GeoJSON format: {"type": "Point", "coordinates": [lon, lat]}
# - Some records have WKT format: "POINT (lon lat)"
#
# =====================================================================================================

import os
import sys
import time
import json
import requests
import pandas as pd
from dotenv import load_dotenv
from requests.exceptions import RequestException
from tqdm import tqdm

# =====================================================================================================
# CONFIGURATION & SETUP
# =====================================================================================================

OCCURRENCES_URL = "https://easin.jrc.ec.europa.eu/apixg2/geo/getoccurrences"
TAKE_LIMIT = 10000
MAX_RETRIES = 5
OUTPUT_FILE = "easin_occurrences_eu.csv"

# EU Country codes (28 countries including Northern Ireland as XI)
EU_COUNTRIES = [
    "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "EL",
    "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK",
    "SI", "ES", "SE", "XI"
]

def get_credentials():
    """
    Get EASIN credentials from environment variables.

    This function is now responsible for loading and validating credentials.
    By raising a ValueError instead of calling sys.exit(), we prevent
    the script from terminating when imported by pytest.

    Raises:
        ValueError: If EASIN_EMAIL or EASIN_PW environment variables are not set.
    """
    load_dotenv()
    email = os.getenv("EASIN_EMAIL")
    password = os.getenv("EASIN_PW")
    if not email or not password:
        raise ValueError("❌ Please set EASIN_EMAIL and EASIN_PW in your .env file.")
    return email, password

# =====================================================================================================
# COORDINATE EXTRACTION UTILITIES
# =====================================================================================================

def extract_coordinates(record):
    """
    Extract latitude and longitude from API response record.
    """
    latitude, longitude = None, None
    
    # Method 1: Try extracting from GeoJSON field
    if 'GeoJSON' in record and record['GeoJSON']:
        try:
            if isinstance(record['GeoJSON'], str):
                geojson = json.loads(record['GeoJSON'])
            else:
                geojson = record['GeoJSON']
            
            if geojson.get('type') == 'Point' and 'coordinates' in geojson:
                coordinates = geojson['coordinates']
                longitude, latitude = coordinates[0], coordinates[1]
                
        except (json.JSONDecodeError, KeyError, IndexError, TypeError) as e:
            pass
    
    # Method 2: Fallback to WKT format if GeoJSON failed
    if latitude is None and longitude is None and 'WKT' in record and record['WKT']:
        try:
            wkt = record['WKT']
            if wkt.startswith('POINT'):
                coords_part = wkt.replace('POINT', '').strip().strip('()')
                coords = coords_part.split()
                if len(coords) == 2:
                    longitude, latitude = float(coords[0]), float(coords[1])
                    
        except (ValueError, IndexError) as e:
            pass
    
    return latitude, longitude

# =====================================================================================================
# API DATA FETCHING
# =====================================================================================================

def fetch_occurrences(species_id, country_code, email, password):
    """
    Fetch all occurrence records for a specific species and country.
    
    The function now accepts email and password as arguments, which makes it
    testable. The main function will get these from get_credentials() and pass them.
    """
    all_records = []
    skip = 0
    retries = 0
    
    while True:
        payload = {
            "Email": email,
            "Password": password,
            "speciesId": str(species_id),
            "countryCode": country_code,
            "dataPartners": "",
            "excludePartners": 0,
            "skip": skip,
            "take": TAKE_LIMIT
        }

        try:
            response = requests.post(
                OCCURRENCES_URL,
                json=payload,
                headers={"Content-Type": "application/json"},
                timeout=60
            )
            response.raise_for_status()
            data = response.json()

            if isinstance(data, dict) and "Empty" in data:
                break
            if isinstance(data, list):
                data = [rec for rec in data if isinstance(rec, dict)]
            if not data:
                break
            all_records.extend(data)
            time.sleep(1)
            
            if len(data) < TAKE_LIMIT:
                break
            skip += TAKE_LIMIT
            retries = 0

        except RequestException as e:
            if retries < MAX_RETRIES:
                retries += 1
                wait_time = 3 * retries
                tqdm.write(f"⚠️ Error fetching {species_id} in {country_code}, retry {retries}/{MAX_RETRIES} in {wait_time}s...")
                time.sleep(wait_time)
                continue
            else:
                tqdm.write(f"❌ Skipping {species_id} in {country_code} after {MAX_RETRIES} failed retries.")
                break

    return all_records

# =====================================================================================================
# DATA PROCESSING & STORAGE
# =====================================================================================================

def save_records(species_id, species_records):
    """
    Process and save occurrence records for a single species to CSV.
    """
    formatted_records = []

    if species_records:
        for rec in species_records:
            if isinstance(rec, dict):
                latitude, longitude = extract_coordinates(rec)
                
                formatted_records.append({
                    "EASIN_ID": species_id,
                    "ScientificName": rec.get("SpeciesName"),
                    "Country": rec.get("CountryId"),
                    "Latitude": latitude,
                    "Longitude": longitude,
                    "DataPartner": rec.get("DataPartnerName"),
                    "Date": rec.get("Year"),
                    "ObservationId": rec.get("ObservationId"),
                    "Reference": rec.get("Reference"),
                    "ReferenceUrl": rec.get("ReferenceUrl")
                })
            else:
                tqdm.write(f"⚠️ Unexpected record type for species {species_id}: {type(rec)}")
    else:
        formatted_records.append({
            "EASIN_ID": species_id,
            "ScientificName": None,
            "Country": None,
            "Latitude": None,
            "Longitude": None,
            "DataPartner": None,
            "Date": None,
            "ObservationId": None,
            "Reference": None,
            "ReferenceUrl": None
        })

    df = pd.DataFrame(formatted_records)
    df.drop_duplicates(inplace=True)
    df.to_csv(OUTPUT_FILE, mode="a", index=False, header=False)

# =====================================================================================================
# MAIN EXECUTION WORKFLOW
# =====================================================================================================

def main():
    """
    Main execution function - orchestrates the entire data fetching process.
    """
    
    # ===== STEP 1: Load Species Data =====
    print("🔄 Loading species data...")
    
    try:
        species_df = pd.read_csv("UnionList_Species_Traits_85_present.csv")
    except FileNotFoundError:
        sys.exit("❌ CSV file 'UnionList_Species_Traits_85_present.csv' not found.")
    
    if species_df is None:
        sys.exit("❌ Failed to load species data from CSV file.")
        
    if "EASIN.ID" not in species_df.columns:
        sys.exit("❌ CSV missing 'EASIN.ID' column.")

    easin_ids = species_df["EASIN.ID"].dropna().unique()
    print(f"🔍 Found {len(easin_ids)} unique species IDs to process.")

    # ===== STEP 2: Setup Output File & Resume Logic =====
    processed_ids = set()
    
    if os.path.exists(OUTPUT_FILE):
        try:
            existing_df = pd.read_csv(OUTPUT_FILE, usecols=["EASIN_ID"])
            processed_ids = set(existing_df["EASIN_ID"].unique())
            print(f"ℹ️ Resuming: {len(processed_ids)} species already processed.")
        except Exception as e:
            print(f"⚠️ Could not read existing output file: {e}\nStarting fresh...")
    else:
        header_df = pd.DataFrame(columns=[
            "EASIN_ID", "ScientificName", "Country", "Latitude", "Longitude",
            "DataPartner", "Date", "ObservationId", "Reference", "ReferenceUrl"
        ])
        header_df.to_csv(OUTPUT_FILE, index=False)
        print(f"✅ Created new output file '{OUTPUT_FILE}' with headers.")

    # Get credentials once at the start
    email, password = get_credentials()

    # ===== STEP 3: Process Each Species =====
    remaining_species = [sid for sid in easin_ids if sid not in processed_ids]
    print(f"📊 Processing {len(remaining_species)} remaining species...")
    
    species_progress = tqdm(
        easin_ids, 
        desc="🦎 Processing species", 
        unit="species",
        colour="green"
    )
    
    total_records_saved = 0
    
    for species_id in species_progress:
        species_progress.set_postfix({"Current": species_id, "Records": total_records_saved})
        
        if species_id in processed_ids:
            continue

        all_country_records = []
        
        country_progress = tqdm(
            EU_COUNTRIES, 
            desc=f"  🌍 Countries for {species_id}", 
            leave=False,
            colour="blue"
        )
        
        for country_code in country_progress:
            country_progress.set_postfix({"Country": country_code})
            country_records = fetch_occurrences(species_id, country_code, email, password)
            all_country_records.extend(country_records)
            time.sleep(1)
        
        if all_country_records:
            save_records(species_id, all_country_records)
            total_records_saved += len(all_country_records)
            tqdm.write(f"✅ Saved {len(all_country_records)} records for species {species_id}")
        else:
            save_records(species_id, [])
            tqdm.write(f"⚠️ No records found for species {species_id}")
        
        time.sleep(1)

    # ===== STEP 4: Completion Summary =====
    print("\n" + "="*80)
    print("🎉 DATA FETCHING COMPLETED!")
    print("="*80)
    print(f"📊 Processed: {len(easin_ids)} species")
    print(f"🌍 Searched: {len(EU_COUNTRIES)} EU countries") 
    print(f"💾 Total records saved: {total_records_saved:,}")
    print(f"📁 Output file: {OUTPUT_FILE}")
    print(f"⏰ Finished at: {time.strftime('%Y-%m-%d %H:%M:%S')}")
    print("="*80)

# =====================================================================================================
# SCRIPT ENTRY POINT
# =====================================================================================================

if __name__ == "__main__":
    print("="*80)
    print("🦎 EASIN Species Occurrence Data Fetcher")
    print("="*80)
    print(f"⏰ Started at: {time.strftime('%Y-%m-%d %H:%M:%S')}")
    print("="*80)
    
    try:
        main()
    except ValueError as e:
        print(e, file=sys.stderr)
        sys.exit(1)
    except KeyboardInterrupt:
        print("\n\n⚠️ Process interrupted by user.")
        print("✅ Data saved up to this point. You can resume by running the script again.")
    except Exception as e:
        print(f"\n❌ Unexpected error: {e}")
        print("✅ Data saved up to this point. You can resume by running the script again.")
        raise