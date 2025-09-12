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
from tqdm import tqdm  # For beautiful progress bars

# =====================================================================================================
# CONFIGURATION & SETUP
# =====================================================================================================

# Load environment variables from .env file
load_dotenv()
EASIN_EMAIL = os.getenv("EASIN_EMAIL")  # Your EASIN account email
EASIN_PW = os.getenv("EASIN_PW")        # Your EASIN account password

# Validate credentials are provided
if not EASIN_EMAIL or not EASIN_PW:
    sys.exit("❌ Please set EASIN_EMAIL and EASIN_PW in your .env file.")

# API Configuration
OCCURRENCES_URL = "https://easin.jrc.ec.europa.eu/apixg2/geo/getoccurrences"  # Main API endpoint
TAKE_LIMIT = 10000      # Max records per API call (pagination limit)
MAX_RETRIES = 5         # Number of retry attempts for failed requests
OUTPUT_FILE = "easin_occurrences_eu.csv"  # Output CSV filename

# EU Country codes (28 countries including Northern Ireland as XI)
EU_COUNTRIES = [
    "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "EL",
    "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK",
    "SI", "ES", "SE", "XI"
]

# =====================================================================================================
# COORDINATE EXTRACTION UTILITIES
# =====================================================================================================

def extract_coordinates(record):
    """
    Extract latitude and longitude from API response record.
    
    The EASIN API provides coordinates in two possible formats:
    1. GeoJSON: {"type": "Point", "coordinates": [longitude, latitude]}
    2. WKT: "POINT (longitude latitude)"
    
    Args:
        record (dict): A single occurrence record from the API
        
    Returns:
        tuple: (latitude, longitude) as floats, or (None, None) if extraction fails
        
    Note:
        - GeoJSON format has coordinates as [lon, lat] (x, y)
        - We return them as (lat, lon) for geographical convention
    """
    latitude, longitude = None, None
    
    # Method 1: Try extracting from GeoJSON field
    if 'GeoJSON' in record and record['GeoJSON']:
        try:
            # Handle both string and dict GeoJSON formats
            if isinstance(record['GeoJSON'], str):
                geojson = json.loads(record['GeoJSON'])  # Parse JSON string
            else:
                geojson = record['GeoJSON']  # Already a dict
            
            # Extract coordinates from Point geometry
            if geojson.get('type') == 'Point' and 'coordinates' in geojson:
                coordinates = geojson['coordinates']
                longitude, latitude = coordinates[0], coordinates[1]  # [lon, lat] format
                
        except (json.JSONDecodeError, KeyError, IndexError, TypeError) as e:
            # GeoJSON parsing failed, will try WKT as fallback
            pass
    
    # Method 2: Fallback to WKT format if GeoJSON failed
    if latitude is None and longitude is None and 'WKT' in record and record['WKT']:
        try:
            wkt = record['WKT']
            # Parse "POINT (longitude latitude)" format
            if wkt.startswith('POINT'):
                coords_part = wkt.replace('POINT', '').strip().strip('()')  # Remove POINT( )
                coords = coords_part.split()  # Split on whitespace
                if len(coords) == 2:
                    longitude, latitude = float(coords[0]), float(coords[1])
                    
        except (ValueError, IndexError) as e:
            # Both methods failed - coordinates will remain None
            pass
    
    return latitude, longitude

# =====================================================================================================
# API DATA FETCHING
# =====================================================================================================

def fetch_occurrences(species_id, country_code=""):
    """
    Fetch all occurrence records for a specific species in a specific country.
    
    Handles:
    - API pagination (automatically fetches all pages)
    - Network failures with exponential backoff retry
    - Empty responses and error messages
    - Rate limiting with delays
    
    Args:
        species_id (str): EASIN species ID (e.g., "R00212")
        country_code (str): Two-letter EU country code (e.g., "AT")
        
    Returns:
        list: List of occurrence records (dicts), empty list if no data or errors
        
    API Response Structure:
    - Success: List of occurrence dicts
    - No data: {"Empty": "There are no results based on your search criteria..."}
    - Error: Various error formats
    """
    all_records = []
    skip = 0          # Pagination offset
    retries = 0       # Current retry count
    
    # Continue fetching until all pages are retrieved
    while True:
        # Prepare API request payload
        payload = {
            "Email": EASIN_EMAIL,           # Authentication
            "Password": EASIN_PW,           # Authentication  
            "speciesId": str(species_id),   # Target species
            "countryCode": country_code,    # Target country
            "dataPartners": "",             # All data partners
            "excludePartners": 0,           # Include all partners
            "skip": skip,                   # Pagination offset
            "take": TAKE_LIMIT              # Records per page
        }

        try:
            # Make API request with timeout
            response = requests.post(
                OCCURRENCES_URL,
                json=payload,
                headers={"Content-Type": "application/json"},
                timeout=60  # 60 second timeout
            )
            response.raise_for_status()  # Raise exception for HTTP errors
            data = response.json()       # Parse JSON response

            # Handle "no results" responses
            if isinstance(data, dict) and "Empty" in data:
                # API returned explicit "no data" message - this is normal
                break

            # Clean and validate response data
            if isinstance(data, list):
                # Remove any non-dictionary items (error messages, etc.)
                data = [rec for rec in data if isinstance(rec, dict)]

            # No more data available
            if not data:
                break

            # Add this page of results to our collection
            all_records.extend(data)

            # Pause 1 second between pages
            time.sleep(1)
            
            # If we got less than the limit, we've reached the end
            if len(data) < TAKE_LIMIT:
                break

            # Prepare for next page
            skip += TAKE_LIMIT
            retries = 0  # Reset retry counter on success

        except RequestException as e:
            # Network error occurred - implement retry logic
            if retries < MAX_RETRIES:
                retries += 1
                wait_time = 3 * retries  # Exponential backoff: 3, 6, 9, 12, 15 seconds
                tqdm.write(f"⚠️ Error fetching {species_id} in {country_code}, retry {retries}/{MAX_RETRIES} in {wait_time}s...")
                time.sleep(wait_time)
                continue
            else:
                # Max retries reached - give up on this species/country combination
                tqdm.write(f"❌ Skipping {species_id} in {country_code} after {MAX_RETRIES} failed retries.")
                break

    return all_records

# =====================================================================================================
# DATA PROCESSING & STORAGE
# =====================================================================================================

def save_records(species_id, species_records):
    """
    Process and save occurrence records for a single species to CSV.
    
    Processing includes:
    - Extracting coordinates from GeoJSON/WKT formats
    - Mapping API field names to standardized column names
    - Handling missing data gracefully
    - Removing duplicate records
    - Appending to existing CSV file
    
    Args:
        species_id (str): EASIN species ID
        species_records (list): List of occurrence record dicts from API
        
    CSV Output Columns:
    - EASIN_ID: Species identifier
    - ScientificName: Species scientific name
    - Country: Two-letter country code  
    - Latitude/Longitude: Decimal coordinates
    - DataPartner: Data source organization
    - Date: Year of observation
    - ObservationId: Unique record identifier
    - Reference: Literature/dataset reference
    - ReferenceUrl: URL to original source
    """
    formatted_records = []

    if species_records:
        # Process each occurrence record
        for rec in species_records:
            if isinstance(rec, dict):
                # Extract coordinates using our helper function
                latitude, longitude = extract_coordinates(rec)
                
                # Map API fields to our standardized format
                formatted_records.append({
                    "EASIN_ID": species_id,
                    "ScientificName": rec.get("SpeciesName"),      # API: SpeciesName
                    "Country": rec.get("CountryId"),               # API: CountryId  
                    "Latitude": latitude,                          # Extracted from GeoJSON/WKT
                    "Longitude": longitude,                        # Extracted from GeoJSON/WKT
                    "DataPartner": rec.get("DataPartnerName"),     # API: DataPartnerName
                    "Date": rec.get("Year"),                       # API: Year
                    "ObservationId": rec.get("ObservationId"),     # API: ObservationId
                    "Reference": rec.get("Reference"),             # API: Reference
                    "ReferenceUrl": rec.get("ReferenceUrl")        # API: ReferenceUrl
                })
            else:
                # Unexpected data type - log warning
                tqdm.write(f"⚠️ Unexpected record type for species {species_id}: {type(rec)}")
    else:
        # No records found - create empty row to mark species as processed
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

    # Create DataFrame and remove duplicates
    df = pd.DataFrame(formatted_records)
    df.drop_duplicates(inplace=True)

    # Append to CSV file (header already exists)
    df.to_csv(OUTPUT_FILE, mode="a", index=False, header=False)

# =====================================================================================================
# MAIN EXECUTION WORKFLOW
# =====================================================================================================

def main():
    """
    Main execution function - orchestrates the entire data fetching process.
    
    Workflow:
    1. Load species list from CSV file
    2. Initialize or resume from existing output file
    3. For each species:
       a. Skip if already processed (resumability)
       b. Fetch data from all EU countries
       c. Save data incrementally
       d. Update progress bars
    4. Display completion summary
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

    # Extract unique species IDs
    easin_ids = species_df["EASIN.ID"].dropna().unique()
    print(f"🔍 Found {len(easin_ids)} unique species IDs to process.")

    # ===== STEP 2: Setup Output File & Resume Logic =====
    processed_ids = set()
    
    if os.path.exists(OUTPUT_FILE):
        # File exists - determine which species are already processed
        try:
            existing_df = pd.read_csv(OUTPUT_FILE, usecols=["EASIN_ID"])
            processed_ids = set(existing_df["EASIN_ID"].unique())
            print(f"ℹ️ Resuming: {len(processed_ids)} species already processed.")
        except Exception as e:
            print(f"⚠️ Could not read existing output file: {e}")
            print("Starting fresh...")
    else:
        # Create new file with header
        header_df = pd.DataFrame(columns=[
            "EASIN_ID", "ScientificName", "Country", "Latitude", "Longitude",
            "DataPartner", "Date", "ObservationId", "Reference", "ReferenceUrl"
        ])
        header_df.to_csv(OUTPUT_FILE, index=False)
        print(f"✅ Created new output file '{OUTPUT_FILE}' with headers.")

    # ===== STEP 3: Process Each Species =====
    
    # Filter out already processed species for progress tracking
    remaining_species = [sid for sid in easin_ids if sid not in processed_ids]
    print(f"📊 Processing {len(remaining_species)} remaining species...")
    
    # Main species processing loop with progress bar
    species_progress = tqdm(
        easin_ids, 
        desc="🦎 Processing species", 
        unit="species",
        colour="green"
    )
    
    total_records_saved = 0
    
    for species_id in species_progress:
        # Update progress bar description with current species
        species_progress.set_postfix({"Current": species_id, "Records": total_records_saved})
        
        # Skip if already processed
        if species_id in processed_ids:
            continue

        # Collect records from all EU countries for this species
        all_country_records = []
        
        # Country processing loop with nested progress bar
        country_progress = tqdm(
            EU_COUNTRIES, 
            desc=f"  🌍 Countries for {species_id}", 
            leave=False,
            colour="blue"
        )
        
        for country_code in country_progress:
            country_progress.set_postfix({"Country": country_code})
            
            # Fetch occurrences for this species in this country
            country_records = fetch_occurrences(species_id, country_code)
            all_country_records.extend(country_records)
            
            # Be polite to the API
            time.sleep(1)
        
        # Save all records for this species
        if all_country_records:
            save_records(species_id, all_country_records)
            total_records_saved += len(all_country_records)
            tqdm.write(f"✅ Saved {len(all_country_records)} records for species {species_id}")
        else:
            # Save empty record to mark as processed
            save_records(species_id, [])
            tqdm.write(f"⚠️ No records found for species {species_id}")
        
        # Brief pause between species to be extra respectful
        time.sleep(1)  # Longer delay between species

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
    except KeyboardInterrupt:
        print("\n\n⚠️ Process interrupted by user.")
        print("✅ Data saved up to this point. You can resume by running the script again.")
    except Exception as e:
        print(f"\n❌ Unexpected error: {e}")
        print("✅ Data saved up to this point. You can resume by running the script again.")
        raise