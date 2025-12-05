# =====================================================
# Flickr Invasive Species Scraper - Fixed & Annotated Version
# =====================================================
# Author: Simon Reynaert (Refactored by Gemini, Fixed by Claude)
# Purpose: Scrape Flickr for geotagged species observations using scientific names
# 
# Key Features:
#   - Searches Flickr using scientific names as tags
#   - Filters by geographic bounding box and date range
#   - Falls back to EXIF data when GPS coordinates are missing
#   - Uses fast local reverse geocoding (reverse_geocoder library)
#   - Single-threaded mode to avoid Windows multiprocessing issues
# =====================================================

import pandas as pd
from datetime import datetime
from tqdm import tqdm  # Progress bars for loops
import time
import os
import numpy as np
import reverse_geocoder as rg  # Local reverse geocoding library
from dotenv import load_dotenv  # For loading API keys from .env file

# --- GLOBAL CONFIGURATION ---
# Load environment variables from .env file (should contain FLICKR_API_KEY and FLICKR_API_SECRET)
load_dotenv()
API_KEY = os.getenv("FLICKR_API_KEY")
API_SECRET = os.getenv("FLICKR_API_SECRET")

# Dictionary defining geographic search boundaries
# Format: region_name: (min_longitude, min_latitude, max_longitude, max_latitude)
# This bounding box covers all of Europe
EU_BOUNDING_BOXES = {
    "EU": (-25, 34, 40, 72)
}

# ----------------------------------
# 1. INITIALIZATION & DATA LOADING
# ----------------------------------

def get_flickr_client():
    """
    Initialize and return an authenticated FlickrAPI client.
    
    The client is used to make API calls to Flickr's search and metadata endpoints.
    
    Returns:
        flickrapi.FlickrAPI: Authenticated Flickr client configured for JSON responses
        
    Raises:
        ValueError: If API_KEY or API_SECRET environment variables are not set
    """
    import flickrapi  # Import here to avoid issues if not installed
    
    # Validate that API credentials exist
    if not API_KEY or not API_SECRET:
        raise ValueError("Flickr API_KEY or API_SECRET missing! Check your .env file.")
    
    # Initialize client with JSON format for easier parsing
    return flickrapi.FlickrAPI(API_KEY, API_SECRET, format='parsed-json')


def load_species_list(csv_path="list_of_union_concern.csv"):
    """
    Load scientific names of species from a CSV file.
    
    Expected CSV format: Must contain a column named "Scientific Name"
    
    Args:
        csv_path (str): Path to the CSV file containing species names
        
    Returns:
        list: List of unique scientific names (strings), empty list if file not found
    """
    try:
        # Read CSV file into a pandas DataFrame
        species_df = pd.read_csv(csv_path)
        
        # Extract the "Scientific Name" column, remove duplicates and missing values
        scientific_names = (
            species_df["Scientific Name"]
            .dropna()  # Remove NaN/null values
            .unique()  # Get only unique names
            .tolist()  # Convert to Python list
        )
        
        print(f"✅ Loaded {len(scientific_names)} species names.")
        return scientific_names
        
    except FileNotFoundError:
        print(f"❌ Error: Species list file not found at {csv_path}")
        return []


# ----------------------------------
# 2. GEOLOCATION HELPERS
# ----------------------------------

def get_exif_coords(flickr_client, photo_id):
    """
    Extract GPS coordinates from a photo's EXIF metadata as a fallback.
    
    Some photos don't have coordinates in the main API response but do have
    GPS data embedded in the image's EXIF metadata. This function retrieves
    that data via an additional API call.
    
    Args:
        flickr_client: Authenticated FlickrAPI client
        photo_id (str): Flickr photo ID
        
    Returns:
        tuple: (latitude, longitude) as strings, or (None, None) if not found
    """
    try:
        # Make API call to retrieve EXIF data for the photo
        exif = flickr_client.photos.getExif(photo_id=photo_id)
        
        # Parse EXIF data to extract GPS-related tags
        # EXIF data contains many technical fields; we only want GPS ones
        gps_tags = {
            item['label']: item['raw']['_content']
            for item in exif.get('photo', {}).get('exif', [])
            if 'GPS' in item.get('label', '')  # Filter for GPS-related labels
        }
        
        # Extract latitude and longitude from the GPS tags
        lat = gps_tags.get('GPS Latitude')
        lon = gps_tags.get('GPS Longitude')
        
        if lat and lon:
            return lat, lon
        return None, None
        
    except Exception:
        # Silently fail if EXIF data can't be retrieved
        # (photo might not have EXIF data or API call failed)
        return None, None


def get_country_from_gps_fast(lat, lon):
    """
    Convert GPS coordinates to a country code using local reverse geocoding.
    
    Uses the reverse_geocoder library which works entirely offline with a
    downloaded database. Much faster than online geocoding APIs.
    
    IMPORTANT: Runs in single-threaded mode (mode=1) to avoid Windows 
    multiprocessing memory issues.
    
    Args:
        lat: Latitude (float or string)
        lon: Longitude (float or string)
        
    Returns:
        str: Two-letter ISO 3166-1 alpha-2 country code (e.g., 'DE', 'FR', 'IT')
        None: If coordinates are invalid or geocoding fails
    """
    # Step 1: Validate and convert coordinates to floats
    try:
        lat = float(lat)
        lon = float(lon)
    except (ValueError, TypeError):
        # Coordinates aren't valid numbers
        return None 
    
    # Step 2: Check for invalid coordinate values
    # - Non-finite (infinity, NaN)
    # - (0, 0) which usually indicates missing data, not Null Island
    if not np.isfinite(lat) or not np.isfinite(lon) or (lat == 0.0 and lon == 0.0):
        return None
    
    try:
        # Step 3: Perform local reverse geocoding lookup
        # mode=1 forces single-threaded operation (critical for Windows)
        # Without mode=1, the library tries to use multiprocessing which causes
        # "paging file too small" errors on Windows
        result = rg.search([(lat, lon)], mode=1)
        
        # Step 4: Extract country code from result
        # Result is a list of dicts; we take the 'cc' (country code) from the first match
        if result and 'cc' in result[0]:
            return result[0]['cc']
            
        return None
        
    except Exception as e:
        # Log errors but don't crash the entire script
        print(f"⚠️ Geocoding error for ({lat}, {lon}): {e}")
        return None


# ----------------------------------
# 3. CORE SCRAPING FUNCTION
# ----------------------------------

def scrape_flickr_data(
    flickr_client,
    species_list,
    bounding_boxes,
    start_date: str = "2004-01-01",  # Flickr launched in 2004
    end_date: str = datetime.now().strftime("%Y-%m-%d"),  # Default to today
):
    """
    Main scraping function: searches Flickr for geotagged photos of species.
    
    How it works:
    1. For each species, searches Flickr using scientific name as tag
    2. Filters by geographic bounding box and date range
    3. Retrieves GPS coordinates from API or EXIF data
    4. Reverse-geocodes coordinates to determine country
    5. Stores all metadata in a pandas DataFrame
    
    Args:
        flickr_client: Authenticated FlickrAPI client
        species_list (list): List of scientific names to search for
        bounding_boxes (dict): Geographic regions to search within
        start_date (str): Start date in YYYY-MM-DD format
        end_date (str): End date in YYYY-MM-DD format
        
    Returns:
        pd.DataFrame: Combined results with columns:
            - photo_id: Flickr photo identifier
            - scientific_name: Species name searched
            - country: Two-letter country code
            - date_taken: When photo was taken
            - latitude, longitude: GPS coordinates
            - url: Original photo URL
            - tags: All tags on the photo
    """
    results = []  # Will store dictionaries, one per photo

    print(f"📅 Scraping data between {start_date} and {end_date}...")

    # Loop through each geographic region (e.g., "EU")
    for region, (min_lon, min_lat, max_lon, max_lat) in bounding_boxes.items():
        
        # Loop through each species with a progress bar
        for sci_name in tqdm(species_list, desc=f"Fetching for {region}"):

            # Prepare multiple tag formats to maximize search results
            # Flickr users might tag as "Homo sapiens", "homosapiens", or "Homo_sapiens"
            name_space = sci_name.strip()  # "Homo sapiens"
            name_flat = name_space.lower().replace(" ", "")  # "homosapiens"
            name_underscore = name_space.replace(" ", "_")  # "Homo_sapiens"
            tags_query = ",".join([name_space, name_flat, name_underscore])

            page = 1  # Start at first page of results
            
            # Pagination loop: keep fetching until no more results
            while True:
                try:
                    # Make API call to search for photos
                    resp = flickr_client.photos.search(
                        tags=tags_query,  # Search by our tag variations
                        tag_mode="any",  # Match ANY of the tags (OR logic)
                        bbox=f"{min_lon},{min_lat},{max_lon},{max_lat}",  # Geographic filter
                        min_taken_date=start_date,  # Date range filter
                        max_taken_date=end_date,
                        has_geo=1,  # Only photos with GPS coordinates
                        extras="geo,date_taken,url_o,tags",  # Additional metadata to retrieve
                        per_page=250,  # Maximum allowed by Flickr API
                        page=page  # Current page number
                    )
                except Exception as e:
                    print(f"⚠️ Error fetching page {page} for {sci_name}: {e}")
                    break  # Skip to next species if API call fails

                # Parse the API response
                photos = resp.get('photos', {})
                photo_list = photos.get('photo', [])
                
                # If no photos on this page, we've reached the end
                if not photo_list:
                    break

                # Process each photo in the current page
                for photo in photo_list:
                    # Try to get GPS coordinates from the main API response
                    lat = photo.get('latitude')
                    lon = photo.get('longitude')
                    image_country = None 

                    # FALLBACK: If coordinates are missing or zero, try EXIF data
                    if not lat or not lon or lat == "0" or lon == "0":
                        lat, lon = get_exif_coords(flickr_client, photo['id'])

                    # Skip photos that couldn't be geolocated at all
                    if not lat or not lon or lat == "0" or lon == "0":
                        continue 

                    # Reverse-geocode the coordinates to get country code
                    image_country = get_country_from_gps_fast(lat, lon)
                    
                    # FALLBACK: If geocoding fails, use the region name (e.g., "EU")
                    # This is a last resort and less precise
                    if not image_country:
                        image_country = region 
                    
                    # Store all photo metadata as a dictionary
                    results.append({
                        'photo_id': photo['id'],
                        'scientific_name': sci_name,
                        'country': image_country,
                        'date_taken': photo.get('datetaken'),
                        'latitude': lat,
                        'longitude': lon,
                        'url': photo.get('url_o'),  # Original size URL
                        'tags': photo.get('tags')
                    })

                # Check if there are more pages to fetch
                total_pages = photos.get('pages', 0)
                if page >= total_pages:
                    break  # No more pages for this species
                
                # Be respectful to Flickr's servers: wait 1 second between requests
                time.sleep(1)
                page += 1  # Move to next page

    # Convert list of dictionaries to a pandas DataFrame and return
    return pd.DataFrame(results)


# ----------------------------------
# 4. MAIN EXECUTION BLOCK
# ----------------------------------

if __name__ == "__main__":
    # This block only runs when the script is executed directly,
    # not when imported as a module
    
    print("🚀 Starting Flickr data scraping (Fixed - Single-threaded Geocoding)...")
    print("-" * 30)

    try:
        # Step 1: Initialize the Flickr API client
        flickr_client = get_flickr_client()
        
        # Step 2: Load the list of species to search for
        species_names = load_species_list()

        # Step 3: Validate that we have species to search for
        if not species_names:
            print("🛑 Cannot proceed without a species list.")
        else:
            # Step 4: Define the date range for the search
            START_DATE = "2023-01-01"
            END_DATE = "2023-12-31"

            # Step 5: Run the main scraping function
            final_results_df = scrape_flickr_data(
                flickr_client=flickr_client,
                species_list=species_names,
                bounding_boxes=EU_BOUNDING_BOXES,
                start_date=START_DATE,
                end_date=END_DATE
            )

            # Step 6: Save results to CSV file
            output_file = f"flickr_species_observations_eu_{START_DATE}_to_{END_DATE}_fastgeo.csv"
            final_results_df.to_csv(output_file, index=False)
            
            # Step 7: Print summary
            print("-" * 30)
            print(f"✅ Process complete. {len(final_results_df)} photos saved to {output_file}.")

    except ValueError as e:
        # Handle missing API credentials
        print(f"🛑 Critical Error: {e}")
    except Exception as e:
        # Catch any other unexpected errors
        print(f"An unexpected error occurred: {e}")