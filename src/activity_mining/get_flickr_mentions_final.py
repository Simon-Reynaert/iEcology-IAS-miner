# =====================================================
# Flickr Invasive Species Scraper for the EU
# =====================================================
# Author: Simon Reynaert
# Purpose: Scrape Flickr for species observations using tags and geotagging.
# Features:
#   - Lazy initialization of the Flickr client (avoids import-time API errors)
#   - Handles EXIF geolocation fallback
#   - Reverse geocoding fallback with geopy
#   - Aggregates results per species per country
# =====================================================

import pandas as pd
from datetime import datetime
from tqdm import tqdm
from geopy.geocoders import Nominatim
import time
import os
from dotenv import load_dotenv

# -------------------------------
# Load environment variables
# -------------------------------
load_dotenv()  # loads API keys from .env

API_KEY = os.getenv("FLICKR_API_KEY")     # Your Flickr API Key
API_SECRET = os.getenv("FLICKR_API_SECRET")  # Your Flickr API Secret

# -------------------------------
# Lazy initialization of Flickr client
# -------------------------------
def get_flickr_client():
    """
    Initialize and return a FlickrAPI client.

    This is done lazily to prevent import-time errors in CI or test environments
    when API keys are not available.

    Returns:
        flickrapi.FlickrAPI: Authenticated Flickr client
    Raises:
        ValueError: if API_KEY or API_SECRET is missing
    """
    import flickrapi  # Import inside function for test safety
    if not API_KEY or not API_SECRET:
        raise ValueError("Flickr API_KEY or API_SECRET missing!")
    return flickrapi.FlickrAPI(API_KEY, API_SECRET, format='parsed-json')


# -------------------------------
# EU Bounding Boxes
# -------------------------------
# Dictionary defining the bounding box for each country/region
# Format: (min_longitude, min_latitude, max_longitude, max_latitude)
eu_bounding_boxes = {
    "EU": (-25, 34, 40, 72)
}

# -------------------------------
# Load species list
# -------------------------------
species_df = pd.read_csv("list_of_union_concern.csv")
scientific_names = (
    species_df["Scientific Name"]
    .dropna()   # Remove empty entries
    .unique()   # Deduplicate
    .tolist()   # Convert to Python list
)

# -------------------------------
# Optional geocoder fallback
# -------------------------------
geolocator = Nominatim(user_agent="geo_flickr_scraper")


def get_country_from_gps(lat, lon):
    """
    Reverse-geocode a latitude/longitude coordinate to a country name.

    Args:
        lat (str or float): Latitude
        lon (str or float): Longitude

    Returns:
        str: Country name if found
        None: if geocoding fails or is unavailable
    """
    try:
        location = geolocator.reverse(f"{lat}, {lon}", language='en')
        address = location.raw.get('address', {})
        return address.get('country')
    except Exception:
        return None


def scrape_flickr_data(flickr_client, geolocator, species_list, bounding_boxes):
    """
    Scrape Flickr for species observations.

    This function searches for multiple tag variants, handles pagination,
    EXIF fallback for missing coordinates, and reverse geocoding.

    Args:
        flickr_client: Initialized FlickrAPI client
        geolocator: geopy Nominatim client for reverse geocoding
        species_list (list): List of species scientific names
        bounding_boxes (dict): Dictionary of country -> bounding box

    Returns:
        pd.DataFrame: Combined results of all scraped observations
    """
    results = []  # Store photo data

    # Iterate over each country bounding box
    for country, (min_lon, min_lat, max_lon, max_lat) in bounding_boxes.items():
        for sci_name in tqdm(species_list, desc=f"Fetching for {country}"):

            # Build multiple tag variants to increase hit rate
            name_space      = sci_name.strip()
            name_flat       = name_space.lower().replace(" ", "")
            name_underscore = name_space.replace(" ", "_")
            tags_query = ",".join([name_space, name_flat, name_underscore])

            page = 1
            while True:
                try:
                    resp = flickr_client.photos.search(
                        tags           = tags_query,
                        tag_mode       = "any",       # match any of the tags
                        bbox           = f"{min_lon},{min_lat},{max_lon},{max_lat}",
                        min_taken_date = "2004-01-01",
                        max_taken_date = datetime.now().strftime("%Y-%m-%d"),
                        has_geo        = 1,
                        extras         = "geo,date_taken,url_o,tags",
                        per_page       = 250,
                        page           = page
                    )
                except Exception as e:
                    print(f"⚠️ Error fetching page {page} for {sci_name}: {e}")
                    break

                photos = resp.get('photos', {})
                photo_list = photos.get('photo', [])
                if not photo_list:
                    break  # No more photos available

                for photo in photo_list:
                    lat = photo.get('latitude')
                    lon = photo.get('longitude')

                    # EXIF fallback if coordinates are missing
                    if not lat or not lon or lat == "0" or lon == "0":
                        try:
                            exif = flickr_client.photos.getExif(photo_id=photo['id'])
                            gps = {
                                item['label']: item['raw']['_content']
                                for item in exif.get('photo', {}).get('exif', [])
                                if 'GPS' in item.get('label', '')
                            }
                            lat = gps.get('GPS Latitude')
                            lon = gps.get('GPS Longitude')
                        except Exception:
                            lat, lon = None, None

                    # Reverse geocoding fallback
                    if not lat or not lon or lat == "0" or lon == "0":
                        image_country = get_country_from_gps(lat, lon)
                    else:
                        image_country = country

                    if not lat or not lon:
                        continue  # Skip photos without coordinates

                    # Store photo info
                    results.append({
                        'photo_id':        photo['id'],
                        'scientific_name': sci_name,
                        'country':         image_country,
                        'date_taken':      photo.get('datetaken'),
                        'latitude':        lat,
                        'longitude':       lon,
                        'url':             photo.get('url_o'),
                        'tags':            photo.get('tags')
                    })

                # Pagination
                if page >= photos.get('pages', 0):
                    break
                print(f"📸 Found {len(photo_list)} photos for {sci_name} on page {page}.")
                
                time.sleep(1)
                page += 1

    return pd.DataFrame(results)


if __name__ == "__main__":
    print("Starting data scraping...")

    # Initialize Flickr client here (lazy)
    flickr = get_flickr_client()

    # Run the scraper
    final_results_df = scrape_flickr_data(
        flickr_client = flickr,
        geolocator    = geolocator,
        species_list  = scientific_names,
        bounding_boxes = eu_bounding_boxes
    )

    # Save results to CSV
    output_file = "flickr_species_observations_eu_combined_latin_normtag_2004-now_test.csv"
    final_results_df.to_csv(output_file, index=False)
    print(f"💾 Process complete. {len(final_results_df)} photos saved to {output_file}.")
