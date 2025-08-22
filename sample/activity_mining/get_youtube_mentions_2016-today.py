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
PROGRESS_FILE = 'progress_yt_2016-now_fuzzymatch.csv'

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

# Define a list of countries with approximate center coordinates and tailored radii.
# This list now incorporates multiple circles for better coverage of larger/irregular countries.
countries = [
    {"country": "Austria",        "location": "47.5162,14.5501",   "radius": "180km"},
    {"country": "Belgium",        "location": "50.5039,4.4699",    "radius": "120km"},
    {"country": "Bulgaria",       "location": "42.7339,25.4858",   "radius": "180km"},
    {"country": "Croatia",        "location": "45.1000,15.2000",   "radius": "200km"},
    {"country": "Croatia",        "location": "43.5000,16.5000",   "radius": "100km"}, # Southern coast/islands
    {"country": "Cyprus",         "location": "35.1264,33.4299",   "radius": "70km"},
    {"country": "Czechia",        "location": "49.8175,15.4730",   "radius": "150km"},
    {"country": "Denmark",        "location": "56.2639,9.5018",    "radius": "180km"},
    {"country": "Denmark",        "location": "55.0000,10.0000",   "radius": "80km"}, # For southern islands
    {"country": "Estonia",        "location": "58.5953,25.0136",   "radius": "150km"},
    {"country": "Finland",        "location": "61.9241,25.7482",   "radius": "350km"},
    {"country": "Finland",        "location": "67.0000,26.0000",   "radius": "200km"}, # Northern Finland
    {"country": "France",         "location": "46.6034,1.8883",    "radius": "350km"},
    {"country": "France",         "location": "48.5000,6.0000",    "radius": "150km"}, # Eastern France
    {"country": "France",         "location": "43.5000,-1.0000",   "radius": "150km"}, # South West
    {"country": "France",         "location": "42.0000,9.0000",    "radius": "70km"},  # Corsica
    {"country": "Germany",        "location": "51.1657,10.4515",   "radius": "300km"},
    {"country": "Germany",        "location": "53.5000,9.5000",    "radius": "150km"}, # North Germany
    {"country": "Germany",        "location": "48.5000,11.5000",   "radius": "150km"}, # South Germany
    {"country": "Greece",         "location": "39.0742,21.8243",   "radius": "250km"},
    {"country": "Greece",         "location": "35.5000,24.0000",   "radius": "150km"}, # Crete and surrounding islands
    {"country": "Greece",         "location": "37.5000,26.0000",   "radius": "150km"}, # Aegean islands
    {"country": "Hungary",        "location": "47.1625,19.5033",   "radius": "150km"},
    {"country": "Ireland",        "location": "53.1424,-7.6921",   "radius": "150km"},
    {"country": "Italy",          "location": "41.8719,12.5674",   "radius": "300km"},
    {"country": "Italy",          "location": "45.5000,10.5000",   "radius": "150km"}, # Northern Italy
    {"country": "Italy",          "location": "38.5000,16.0000",   "radius": "150km"}, # Southern Italy/Sicily
    {"country": "Italy",          "location": "40.0000,9.0000",    "radius": "120km"}, # Sardinia
    {"country": "Latvia",         "location": "56.8796,24.6032",   "radius": "150km"},
    {"country": "Lithuania",      "location": "55.1694,23.8813",   "radius": "150km"},
    {"country": "Luxembourg",     "location": "49.8153,6.1296",    "radius": "50km"},
    {"country": "Malta",          "location": "35.9375,14.3754",   "radius": "20km"},
    {"country": "Netherlands",    "location": "52.1326,5.2913",    "radius": "100km"},
    {"country": "Poland",         "location": "51.9194,19.1451",   "radius": "250km"},
    {"country": "Poland",         "location": "53.5000,22.0000",   "radius": "150km"}, # Eastern Poland
    {"country": "Portugal",       "location": "39.3999,-8.2245",   "radius": "200km"},
    {"country": "Portugal",       "location": "38.7000,-28.0000",  "radius": "150km"}, # Azores (approximation)
    {"country": "Portugal",       "location": "32.7000,-17.0000",  "radius": "100km"}, # Madeira (approximation)
    {"country": "Romania",        "location": "45.9432,24.9668",   "radius": "250km"},
    {"country": "Romania",        "location": "46.0000,28.0000",   "radius": "100km"}, # Eastern part
    {"country": "Slovakia",       "location": "48.6690,19.6990",   "radius": "150km"},
    {"country": "Slovenia",       "location": "46.1512,14.9955",   "radius": "100km"},
    {"country": "Spain",          "location": "40.4637,-3.7492",   "radius": "400km"},
    {"country": "Spain",          "location": "42.5000,-7.5000",   "radius": "150km"}, # Northwest (Galicia)
    {"country": "Spain",          "location": "37.0000,-4.0000",   "radius": "150km"}, # Southern Spain
    {"country": "Spain",          "location": "28.5000,-16.0000",  "radius": "150km"}, # Canary Islands (approximation)
    {"country": "Spain",          "location": "39.7000,2.9000",    "radius": "100km"}, # Balearic Islands
    {"country": "Sweden",         "location": "60.1282,18.6435",   "radius": "400km"},
    {"country": "Sweden",         "location": "65.0000,18.0000",   "radius": "300km"}, # Northern Sweden
    {"country": "Sweden",         "location": "56.0000,14.0000",   "radius": "150km"}, # Southern Sweden
    {"country": "United Kingdom", "location": "54.0000,-2.5000",   "radius": "300km"},
    {"country": "United Kingdom", "location": "51.0000,0.0000",    "radius": "150km"}, # South East England
    {"country": "United Kingdom", "location": "57.0000,-4.5000",   "radius": "200km"}, # Scotland
    {"country": "United Kingdom", "location": "54.5000,-7.0000",   "radius": "80km"},  # Northern Ireland
    {"country": "Switzerland",    "location": "46.8182,8.2275",    "radius": "100km"},
    {"country": "Norway",         "location": "60.4720,8.4689",    "radius": "400km"},
    {"country": "Norway",         "location": "67.0000,14.0000",   "radius": "300km"}, # Mid-Norway
    {"country": "Norway",         "location": "70.0000,24.0000",   "radius": "200km"}, # Northern Norway
    {"country": "Norway",         "location": "59.0000,10.0000",   "radius": "100km"}  # Southern Coast
]

# Default query date range – change these as needed.
DEFAULT_PUBLISHED_AFTER = "2016-01-01T00:00:00Z"
DEFAULT_PUBLISHED_BEFORE = "2025-07-16T00:00:00Z"

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
                    # Post-filtering for exact match in title or description (case-insensitive) THIS IS IF WE WANT EXACT MATCHES, BUT MIGHT LEAD TO MISSED VIDEOS
                    # if query.lower() not in title.lower() and query.lower() not in description.lower():
                    # continue
                    all_video_data.append({
                        "video_id": video_id,
                        "title": title,
                        "description": description,
                        "published_at": item['snippet']['publishedAt'],
                        "channel_title": item['snippet']['channelTitle'],
                        "url": f"https://www.youtube.com/watch?v={video_id}",
                        "country": country_info["country"], # This will now reflect the *specific* country name for each circle
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
        os.makedirs('youtube_results_2016-now_fuzzymatch', exist_ok=True)
        # Modified file name to append a hash or unique identifier if multiple circles for a country
        # This modification is crucial to avoid overwriting if a country has multiple circles
        # and you want to distinguish results from each specific circle.
        # However, for your current setup, where all results for a *country name*
        # are aggregated before saving, the original filename might be sufficient
        # if the goal is one CSV per (species, country) pair, regardless of the circle used.
        # Let's stick to the original naming for now, assuming you want all videos for a (species, country)
        # to be in one file, even if found via different circles.
        file_name = f"youtube_results_2016-now_fuzzymatch/{species.replace(' ', '_')}_{country.replace(' ', '_')}.csv"
        
        # Check if file exists to append or create
        if os.path.exists(file_name):
            existing_df = pd.read_csv(file_name)
            # Use pd.concat and drop_duplicates to avoid duplicate entries if running multiple circles
            # for the same country and species in subsequent runs, or if a video
            # falls into overlapping circles.
            combined_df = pd.concat([existing_df, video_df]).drop_duplicates(subset=['video_id']).reset_index(drop=True)
            combined_df.to_csv(file_name, index=False)
            print(f"Appended and de-duplicated results to {file_name}")
        else:
            video_df.to_csv(file_name, index=False)
            print(f"Progress saved to {file_name}")

def get_video_data_for_species_and_countries(species_list, country_list):
    progress = load_progress()
    
    # Create a dictionary to temporarily store results for a given (species, country)
    # This is to aggregate results from multiple circles for the same country before saving.
    current_species_country_results = {}

    for species in species_list:
        print(f"\n🔍 Searching for species: {species}")
        
        # Group country_list by actual country name for progress tracking and aggregation
        grouped_countries = {}
        for country_info in country_list:
            country_name = country_info['country']
            if country_name not in grouped_countries:
                grouped_countries[country_name] = []
            grouped_countries[country_name].append(country_info)

        for country_name, locations_info in grouped_countries.items():
            # Check progress based on the overall country name
            if not progress.empty and ((progress['species'] == species) & (progress['country'] == country_name)).any():
                print(f"  ⚡️ Skipping {species} in {country_name} (already processed)")
                continue

            print(f"  📍 Processing country: {country_name}")
            
            # Initialize or retrieve aggregated results for this species-country pair
            # Use a tuple (species, country_name) as key to store aggregated data
            if (species, country_name) not in current_species_country_results:
                current_species_country_results[(species, country_name)] = []

            for country_info in locations_info:
                print(f"    Searching in location: {country_info['location']}, Radius: {country_info['radius']}")
                
                # First, try the default full date range query.
                results, fetched_count = fetch_videos(species, country_info, DEFAULT_PUBLISHED_AFTER, DEFAULT_PUBLISHED_BEFORE)
                if results is None:
                    # Quota error encountered; save what we have and stop.
                    # This means we save the aggregated results for the current (species, country)
                    # collected so far from previous circles or intervals.
                    save_progress(current_species_country_results[(species, country_name)], species, country_name)
                    return # Stop the entire script
                
                # Add results from this circle to the aggregated list for the country
                current_species_country_results[(species, country_name)].extend(results)

                # If results are within the 500 limit, use these.
                # Note: This logic needs a slight adjustment for multiple circles.
                # We need to consider the *total* fetched for this specific circle's query,
                # not the overall aggregated total.
                if fetched_count >= 500: # If this specific circle query hit the cap
                    print(f"    ⚠️  Hit 500-result cap for this circle. Splitting query into smaller intervals (100 days).")
                    start_dt = datetime.strptime(DEFAULT_PUBLISHED_AFTER, "%Y-%m-%dT%H:%M:%SZ")
                    end_dt   = datetime.strptime(DEFAULT_PUBLISHED_BEFORE, "%Y-%m-%dT%H:%M:%SZ")
                    intervals = split_time_range(start_dt, end_dt, delta_days=100)
                    for pub_after, pub_before in intervals:
                        print(f"      Interval: {pub_after} to {pub_before}")
                        sub_results, _ = fetch_videos(species, country_info, pub_after, pub_before)
                        if sub_results is None:
                            # Quota error; save current aggregated results and stop
                            save_progress(current_species_country_results[(species, country_name)], species, country_name)
                            return
                        current_species_country_results[(species, country_name)].extend(sub_results)
                        time.sleep(1) # Be mindful of API rate limits
                time.sleep(1) # Delay between circles for the same country

            # After processing all circles for a given country for the current species
            # Save the aggregated (and de-duplicated) results for this country.
            final_country_results_df = pd.DataFrame(current_species_country_results[(species, country_name)])
            if not final_country_results_df.empty:
                # Deduplicate again before saving to catch any duplicates from overlapping circles
                final_country_results_df.drop_duplicates(subset=['video_id'], inplace=True)
                save_progress(final_country_results_df.to_dict('records'), species, country_name)
            else:
                print(f"No videos found for {species} in {country_name} across all circles.")
            
            # Update progress for the *country name* (not individual circles)
            update_progress(species, country_name)
            time.sleep(1) # Delay between countries for the same species

# Run the main function
get_video_data_for_species_and_countries(species_list, countries)