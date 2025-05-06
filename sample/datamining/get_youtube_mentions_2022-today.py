import pandas as pd
from googleapiclient.discovery import build
from googleapiclient.errors import HttpError
import time
import os
from dotenv import load_dotenv
from datetime import datetime, timedelta

# Define and read your YouTube Data API key (saved as environment variable)
load_dotenv() #loads from .env
API_KEY = os.getenv("YT_API_KEY") 
#print(f"API Key: {API_KEY}")
youtube = build('youtube', 'v3', developerKey=API_KEY)

# File to track progress
PROGRESS_FILE = 'progress_yt.csv'

def load_progress():
    """Read progress CSV if it exists, else create an empty DataFrame."""
    if os.path.exists(PROGRESS_FILE):
        return pd.read_csv(PROGRESS_FILE)
    else:
        return pd.DataFrame(columns=["species", "country", "timestamp"])

def update_progress(species, country):
    """Append a species-country combination to the progress CSV with the current timestamp."""
    progress_df = load_progress()
    new_row = pd.DataFrame({
        "species": [species],
        "country": [country],
        "timestamp": [datetime.now().strftime("%Y-%m-%d %H:%M:%S")]
    })
    progress_df = pd.concat([progress_df, new_row], ignore_index=True)
    progress_df.to_csv(PROGRESS_FILE, index=False)
    print(f"Updated progress: {species} in {country}")

# Load species list (exact matches)
species_df = pd.read_csv('list_of_union_concern.csv')
species_list = species_df['Scientific Name'].dropna().unique().tolist()

# Define a list of countries (EU members plus additional countries) 
# with approximate center coordinates and a uniform radius.
countries = [
    #{"country": "Austria",      "location": "47.5162,14.5501",  "radius": "50km"},
    #{"country": "Belgium",      "location": "50.5039,4.4699",   "radius": "50km"},
    #{"country": "Bulgaria",     "location": "42.7339,25.4858",  "radius": "50km"},
    #{"country": "Croatia",      "location": "45.1000,15.2000",  "radius": "50km"},
    #{"country": "Cyprus",       "location": "35.1264,33.4299",  "radius": "50km"},
    #{"country": "Czechia",      "location": "49.8175,15.4730",  "radius": "50km"},
    #{"country": "Denmark",      "location": "56.2639,9.5018",   "radius": "50km"},
    #{"country": "Estonia",      "location": "58.5953,25.0136",  "radius": "50km"},
    #{"country": "Finland",      "location": "61.9241,25.7482",  "radius": "50km"},
    {"country": "France",       "location": "46.6034,1.8883",   "radius": "150km"},
    {"country": "Germany",      "location": "51.1657,10.4515",  "radius": "150km"},
    #{"country": "Greece",       "location": "39.0742,21.8243",  "radius": "50km"},
    #{"country": "Hungary",      "location": "47.1625,19.5033",  "radius": "50km"},
    #{"country": "Ireland",      "location": "53.1424,-7.6921",  "radius": "50km"},
    #{"country": "Italy",        "location": "41.8719,12.5674",  "radius": "50km"},
    #{"country": "Latvia",       "location": "56.8796,24.6032",  "radius": "50km"},
    #{"country": "Lithuania",    "location": "55.1694,23.8813",  "radius": "50km"},
    #{"country": "Luxembourg",   "location": "49.8153,6.1296",   "radius": "50km"},
    #{"country": "Malta",        "location": "35.9375,14.3754",  "radius": "50km"},
    #{"country": "Netherlands",  "location": "52.1326,5.2913",   "radius": "50km"},
    {"country": "Poland",       "location": "51.9194,19.1451",  "radius": "150km"},
    #{"country": "Portugal",     "location": "39.3999,-8.2245",  "radius": "50km"},
    {"country": "Romania",      "location": "45.9432,24.9668",  "radius": "150km"},
    #{"country": "Slovakia",     "location": "48.6690,19.6990",  "radius": "50km"},
    #{"country": "Slovenia",     "location": "46.1512,14.9955",  "radius": "50km"},
    {"country": "Spain",        "location": "40.4637,-3.7492",  "radius": "150km"},
    #{"country": "Sweden",       "location": "60.1282,18.6435",  "radius": "50km"},
    # Additional countries outside the EU
    #{"country": "United Kingdom", "location": "55.3781,-3.4360", "radius": "50km"},
    #{"country": "Switzerland",    "location": "46.8182,8.2275",  "radius": "50km"},
    #{"country": "Norway",         "location": "60.4720,8.4689",  "radius": "50km"}
]

# Default query date range – change these as needed.
DEFAULT_PUBLISHED_AFTER = "2022-01-01T00:00:00Z"
DEFAULT_PUBLISHED_BEFORE = "2025-04-15T00:00:00Z"

def search_youtube_geo(query, location, radius, published_after, published_before, next_page_token=None):
    request = youtube.search().list(
        part="snippet",
        q=f'"{query}"',  # Exact match: species enclosed in quotes
        type="video",
        maxResults=50,
        pageToken=next_page_token,
        location=location,
        locationRadius=radius,
        publishedAfter=published_after,
        publishedBefore=published_before
    )
    try:
        response = request.execute()
    except HttpError as e:
        if e.resp.status == 403 and 'quota' in str(e).lower():
            print("Quota limit exceeded. Saving progress and stopping.")
            return None
        else:
            raise e
    return response

def fetch_videos(query, country_info, published_after, published_before):
    """Fetch videos using pagination and return both results and count."""
    all_video_data = []
    page_token = None
    total_fetched = 0

    while True:
        response = search_youtube_geo(query, country_info["location"], country_info["radius"],
                                      published_after, published_before, page_token)
        if response is None:
            return None, total_fetched  # Quota issue encountered

        for item in response.get("items", []):
            if item['id']['kind'] == 'youtube#video':
                video_id = item['id'].get('videoId')
                if video_id:
                    title = item['snippet']['title']
                    description = item['snippet']['description']
                    # Post-filtering for exact match in title or description (case-insensitive)
                    if query.lower() not in title.lower() and query.lower() not in description.lower():
                        continue
                    all_video_data.append({
                        "video_id": video_id,
                        "title": title,
                        "description": description,
                        "published_at": item['snippet']['publishedAt'],
                        "channel_title": item['snippet']['channelTitle'],
                        "url": f"https://www.youtube.com/watch?v={video_id}",
                        "country": country_info["country"],
                        "species": query
                    })
        total_fetched += len(response.get("items", []))
        page_token = response.get("nextPageToken")
        if not page_token or total_fetched >= 500:
            break
        time.sleep(1)
    return all_video_data, total_fetched

def split_time_range(start, end, delta_days=100):
    """Split the period [start, end) into sub-intervals of delta_days."""
    intervals = []
    current = start
    while current < end:
        next_interval = current + timedelta(days=delta_days)
        if next_interval > end:
            next_interval = end
        intervals.append((current.isoformat("T") + "Z", next_interval.isoformat("T") + "Z"))
        current = next_interval
    return intervals

def save_progress(video_data, species, country):
    if video_data:
        video_df = pd.DataFrame(video_data)
        os.makedirs('youtube_results_test', exist_ok=True)
        file_name = f"youtube_results_test/{species.replace(' ', '_')}_{country.replace(' ', '_')}.csv"
        video_df.to_csv(file_name, index=False)
        print(f"Progress saved to {file_name}")

def get_video_data_for_species_and_countries(species_list, country_list):
    progress = load_progress()
    for species in species_list:
        print(f"\n🔍 Searching for species: {species}")
        for country_info in country_list:
            country_name = country_info['country']
            # Skip species/country combination if already processed.
            if not progress.empty and ((progress['species'] == species) & (progress['country'] == country_name)).any():
                print(f"  ⚡️ Skipping {species} in {country_name} (already processed)")
                continue

            print(f"  📍 In country: {country_name} (Location: {country_info['location']}, Radius: {country_info['radius']})")
            # First, try the default full date range query.
            results, fetched_count = fetch_videos(species, country_info, DEFAULT_PUBLISHED_AFTER, DEFAULT_PUBLISHED_BEFORE)
            if results is None:
                # Quota error encountered; save what we have and stop.
                save_progress([], species, country_name)
                return
            # If results are within the 500 limit, use these.
            if fetched_count < 500:
                save_progress(results, species, country_name)
            else:
                # More than or equal to 500 videos: break into smaller time windows.
                print("  ⚠️  Hit 500-result cap. Splitting query into smaller intervals (100 days).")
                all_results = []
                start_dt = datetime.strptime(DEFAULT_PUBLISHED_AFTER, "%Y-%m-%dT%H:%M:%SZ")
                end_dt   = datetime.strptime(DEFAULT_PUBLISHED_BEFORE, "%Y-%m-%dT%H:%M:%SZ")
                intervals = split_time_range(start_dt, end_dt, delta_days=100)
                for pub_after, pub_before in intervals:
                    print(f"    Interval: {pub_after} to {pub_before}")
                    sub_results, _ = fetch_videos(species, country_info, pub_after, pub_before)
                    if sub_results is None:
                        # Quota error; save what we have so far
                        save_progress(all_results, species, country_name)
                        return
                    all_results.extend(sub_results)
                    time.sleep(1)
                save_progress(all_results, species, country_name)
            # Once processed, update progress.
            update_progress(species, country_name)
            time.sleep(1)

# Run the main function
get_video_data_for_species_and_countries(species_list, countries)