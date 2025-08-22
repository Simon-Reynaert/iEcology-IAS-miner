#load dependencies
import flickrapi
import pandas as pd
from datetime import datetime
from tqdm import tqdm
from geopy.geocoders import Nominatim
import webbrowser
import time
import os
from dotenv import load_dotenv

# load API credentials 
load_dotenv() #loads from .env

API_KEY = os.getenv("FLICKR_API_KEY")
API_SECRET = os.getenv("FLICKR_API_SECRET")

print(f"API Key: {API_KEY}")
print(f"API Secret: {API_SECRET}")

# Initialize flickrapi client
flickr = flickrapi.FlickrAPI(API_KEY, API_SECRET, format='parsed-json')

# Authenticate via OAuth if needed (otherwise ommitted, should only need this once)
if not flickr.token_valid(perms='read'):
    flickr.get_request_token(oauth_callback='oob')
    authorize_url = flickr.auth_url(perms='read')
    print(f"👉 Authorize this app at:\n{authorize_url}\n")
    webbrowser.open_new_tab(authorize_url)
    verifier = input("🔐 Enter verifier code from Flickr: ")
    flickr.get_access_token(verifier)

# EU country bounding boxes (min_lon, min_lat, max_lon, max_lat)
eu_bounding_boxes = {
    "EU": (-25, 34, 40, 72)  # Bounding box for the entire EU
}

# Load species list
species_df = pd.read_csv("list_of_union_concern.csv")
scientific_names = (
    species_df["Scientific Name"]
    .dropna()  # remove blanks
    .unique()  # deduplicate
    .tolist()  # convert to list
)

#Optional geocoder fallback to get country from GPS coordinates
geolocator = Nominatim(user_agent="geo_flickr_scraper")


def get_country_from_gps(lat, lon):
    """Reverse‐geocode a (lat, lon) pair to find the country name."""
    try:
        location = geolocator.reverse(f"{lat}, {lon}", language='en')
        address = location.raw.get('address', {})
        return address.get('country')
    except Exception:
        return None

# Fetching loop (machine‐tags + EXIF + reverse‐geocode fallback + error handling)
results = [] #initialize empty list to store results
# inside your country/species loops, replace the flickr.photos.search block with this

for country, (min_lon, min_lat, max_lon, max_lat) in eu_bounding_boxes.items():
    for sci_name in tqdm(scientific_names, desc=f"Fetching for {country}"):

        # build tag variants
        name_space      = sci_name.strip()                     # "Axis axis"
        name_flat       = name_space.lower().replace(" ", "")  # "axisaxis"
        name_underscore = name_space.replace(" ", "_")         # "Axis_axis"

        tags_query = ",".join([name_space, name_flat, name_underscore])

        page = 1
        while True:
            try:
                resp = flickr.photos.search(
                    # using normal tags since best results 
                    tags     = tags_query,
                    tag_mode = "any",       # return photos matching any of the above
                    bbox             = f"{min_lon},{min_lat},{max_lon},{max_lat}",
                    min_taken_date   = "2004-01-01",
                    max_taken_date   = datetime.now().strftime("%Y-%m-%d"), #REMOVE # IN AUTOMATED WORKFLOW!
                    #max_taken_date   = "2023-12-31", # Adjust as needed
                    has_geo          = 1,
                    extras           = "geo,date_taken,url_o,tags",
                    per_page         = 250,
                    page             = page
                )
            except Exception as e:
                print(f"⚠️ Error fetching page {page} for {sci_name}: {e}")
                break

            photos     = resp.get('photos', {})
            photo_list = photos.get('photo', [])
            if not photo_list:
                break  # no more photos for this species

            for photo in photo_list:
                lat = photo.get('latitude')
                lon = photo.get('longitude')

                # EXIF fallback
                if not lat or not lon or lat == "0" or lon == "0":
                    try:
                        exif = flickr.photos.getExif(photo_id=photo['id'])
                        gps  = {
                            item['label']: item['raw']['_content']
                            for item in exif.get('photo', {}).get('exif', [])
                            if 'GPS' in item.get('label', '')
                        }
                        lat = gps.get('GPS Latitude')
                        lon = gps.get('GPS Longitude')
                    except Exception:
                        lat, lon = None, None

                # reverse-geocode fallback
                if not lat or not lon or lat == "0" or lon == "0":
                    image_country = get_country_from_gps(lat, lon)
                else:
                    image_country = country

                if not lat or not lon:
                    continue

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

            if page >= photos.get('pages', 0):
                break
            print(f"📸 Found {len(photo_list)} photos for {sci_name} on page {page}.")

            
            time.sleep(1)
            page += 1
    
        # Save progress periodically
        df = pd.DataFrame(results)
        output_file = "flickr_species_observations_eu_combined_latin_normtag_2004-now.csv" #adjust as needed
        df.to_csv(output_file, index=False)
        print(f"💾 Progress saved — {len(df)} photos so far.")