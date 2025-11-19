"""
YouTube Geolocated Video Fetcher
==================================
Fetches videos from YouTube Data API v3 based on geographic location circles.
Can be used in notebooks (call functions directly) or as a standalone script.

Main Functions:
- fetch_videos(): Fetch videos for a single query/location/time range
- get_video_data_for_species_and_countries(): Main orchestration function
- search_youtube_geo(): Low-level API search wrapper

Progress Tracking:
- Uses CSV file to track completed species-country combinations
- Allows resuming after quota exhaustion or interruption

Note on Pagination:
- YouTube API returns max 50 results per page
- Maximum 500 results total per query (10 pages)
- Uses nextPageToken for pagination across all available pages
"""

import pandas as pd
from googleapiclient.discovery import build
from googleapiclient.errors import HttpError
import time
import os
from dotenv import load_dotenv
from datetime import datetime, timedelta


# =============================================================================
# PROGRESS TRACKING FUNCTIONS
# =============================================================================

def load_progress(progress_file):
    """
    Load progress from CSV file or create empty DataFrame.
    
    :param progress_file: Path to progress CSV file
    :type progress_file: str
    :return: Progress data with columns [species, country, timestamp]
    :rtype: pd.DataFrame
    """
    if os.path.exists(progress_file):
        return pd.read_csv(progress_file)
    else:
        return pd.DataFrame(columns=["species", "country", "timestamp"])


def update_progress(species, country, progress_file):
    """
    Append completed species-country combination to progress file.
    
    :param species: Scientific name of species
    :type species: str
    :param country: Country name
    :type country: str
    :param progress_file: Path to progress CSV file
    :type progress_file: str
    :return: None
    :rtype: None
    """
    progress_df = load_progress(progress_file)
    new_row = pd.DataFrame({
        "species": [species],
        "country": [country],
        "timestamp": [datetime.now().strftime("%Y-%m-%d %H:%M:%S")]
    })
    progress_df = pd.concat([progress_df, new_row], ignore_index=True)
    progress_df.to_csv(progress_file, index=False)
    print(f"✓ Updated progress: {species} in {country}")


# =============================================================================
# YOUTUBE API FUNCTIONS
# =============================================================================

def search_youtube_geo(youtube, query, location, radius, published_after, 
                       published_before, next_page_token=None):
    """
    Execute a single YouTube API search request with geographic constraints.
    
    This function makes one API call to YouTube's search endpoint. It supports
    pagination through the next_page_token parameter. The API returns a maximum
    of 50 results per call.
    
    :param youtube: Initialized YouTube Data API client
    :type youtube: googleapiclient.discovery.Resource
    :param query: Search query (species name, will be wrapped in quotes for exact match)
    :type query: str
    :param location: Center point as "latitude,longitude"
    :type location: str
    :param radius: Search radius (e.g., "100km")
    :type radius: str
    :param published_after: Start date in ISO 8601 format (e.g., "2020-01-01T00:00:00Z")
    :type published_after: str
    :param published_before: End date in ISO 8601 format (e.g., "2024-01-01T00:00:00Z")
    :type published_before: str
    :param next_page_token: Token for fetching next page of results, defaults to None
    :type next_page_token: str, optional
    :raises HttpError: For non-quota API errors (e.g., invalid parameters, auth errors)
    :return: API response dict containing video results and pagination info, or None if quota exceeded
    :rtype: dict or None
    """
    request = youtube.search().list(
        part="snippet",
        q=f'"{query}"',  # Exact match search
        type="video",
        maxResults=50,  # Maximum allowed per page
        pageToken=next_page_token,
        location=location,
        locationRadius=radius,
        publishedAfter=published_after,
        publishedBefore=published_before
    )
    
    try:
        response = request.execute()
        return response
    except HttpError as e:
        if e.resp.status == 403 and 'quota' in str(e).lower():
            print("⚠️  API quota limit exceeded. Stopping execution.")
            return None
        else:
            raise e


def fetch_videos(youtube, query, country_info, published_after, published_before):
    """
    Fetch all available videos for a query/location/time range with full pagination.
    
    This function iterates through ALL available pages of results using the nextPageToken
    until either:
    - No more pages are available (nextPageToken is None)
    - The 500-result limit is reached (YouTube API limitation: max 10 pages × 50 results)
    
    The function captures YouTube's totalResults estimate from the first API response,
    which represents the platform's estimate of matching videos for this specific
    circle/time range combination.
    
    Pagination Logic:
    - Calls search_youtube_geo() repeatedly with nextPageToken
    - Each call returns up to 50 results
    - Continues until nextPageToken is None or 500 results fetched
    - Includes 1-second delay between pages for rate limiting
    
    :param youtube: Initialized YouTube Data API client
    :type youtube: googleapiclient.discovery.Resource
    :param query: Search query (species name)
    :type query: str
    :param country_info: Dictionary with keys 'country', 'location', 'radius'
    :type country_info: dict
    :param published_after: Start date in ISO 8601 format
    :type published_after: str
    :param published_before: End date in ISO 8601 format
    :type published_before: str
    :return: Tuple of (video_data_list, fetched_count, total_results_estimate)
        - video_data_list: List of video dicts with complete metadata
        - fetched_count: Number of videos actually fetched in this call
        - total_results_estimate: YouTube's estimate from first response (max 1,000,000)
        Returns (None, fetched_count, estimate) if quota exceeded
    :rtype: tuple(list[dict], int, int) or tuple(None, int, int)
    """
    all_video_data = []
    page_token = None
    total_fetched = 0
    total_results_estimate = 0

    # Pagination loop - continues until no more pages or hit 500-result cap
    while True:
        response = search_youtube_geo(
            youtube, query, country_info["location"], country_info["radius"],
            published_after, published_before, page_token
        )
        
        if response is None:
            # Quota exceeded
            return None, total_fetched, total_results_estimate
        
        # Capture estimate from first response of this circle/interval
        if total_fetched == 0:
            total_results_estimate = response.get("pageInfo", {}).get("totalResults", 0)
        
        # Process video items from this page
        for item in response.get("items", []):
            if item['id']['kind'] == 'youtube#video':
                video_id = item['id'].get('videoId')
                if video_id:
                    all_video_data.append({
                        "video_id": video_id,
                        "title": item['snippet']['title'],
                        "description": item['snippet']['description'],
                        "published_at": item['snippet']['publishedAt'],
                        "channel_title": item['snippet']['channelTitle'],
                        "url": f"https://www.youtube.com/watch?v={video_id}",
                        "country": country_info["country"],
                        "species": query,
                        "location": country_info["location"],
                        "radius": country_info["radius"],
                        "published_after": published_after,
                        "published_before": published_before,
                        "total_results_estimate": total_results_estimate
                    })
        
        total_fetched += len(response.get("items", []))
        
        # Get token for next page (None if no more pages exist)
        page_token = response.get("nextPageToken")
        
        # Exit conditions:
        # 1. No more pages available (page_token is None)
        # 2. Hit 500-result cap (YouTube API limitation: 10 pages max)
        if not page_token or total_fetched >= 500:
            break
        
        time.sleep(1)  # Rate limiting between pages
    
    return all_video_data, total_fetched, total_results_estimate


def should_requery(fetched_count, estimate, threshold_ratio=0.7):
    """
    Determine if a query should be re-run with time splitting to capture missing videos.
    
    This function implements logic to detect when YouTube's pagination limit (500 results)
    has prevented us from fetching all available videos. It compares the actual fetched
    count against YouTube's totalResults estimate to identify significant gaps.
    
    Decision Logic:
    - Skip if estimate is small (≤50) - not worth re-querying
    - Skip if we got everything (fetched ≥ estimate)
    - Skip if we got "close enough" (≥70% of estimate by default)
    - Requery if we hit 500-result cap AND estimate suggests 1.5× more videos exist
    
    :param fetched_count: Number of videos actually fetched
    :type fetched_count: int
    :param estimate: YouTube's totalResults estimate
    :type estimate: int
    :param threshold_ratio: Minimum ratio of fetched/estimate to accept, defaults to 0.7
    :type threshold_ratio: float, optional
    :return: True if query should be re-run with time splitting
    :rtype: bool
    """
    # Don't requery if estimate is very small or we got everything
    if estimate <= 50 or fetched_count >= estimate:
        return False
    
    # Don't requery if we got close enough to the estimate
    if estimate > 0 and (fetched_count / estimate) >= threshold_ratio:
        return False
    
    # Requery if we hit the cap but estimate suggests many more videos exist
    if fetched_count >= 500 and estimate > fetched_count * 1.5:
        return True
    
    return False


# =============================================================================
# TIME SPLITTING UTILITY
# =============================================================================

def split_time_range(start_datetime, end_datetime, delta_days=100):
    """
    Split a time range into smaller intervals for handling large result sets.
    
    When a query returns 500+ results, splitting the time range allows us to
    fetch more videos by making multiple queries, each covering a smaller time period.
    This works around YouTube's 500-result pagination limit.
    
    :param start_datetime: Start of period
    :type start_datetime: datetime.datetime
    :param end_datetime: End of period
    :type end_datetime: datetime.datetime
    :param delta_days: Days per interval, defaults to 100
    :type delta_days: int, optional
    :return: List of (start_iso, end_iso) tuples in ISO 8601 format with 'Z' suffix
    :rtype: list[tuple(str, str)]
    """
    intervals = []
    current = start_datetime
    
    while current < end_datetime:
        next_interval = current + timedelta(days=delta_days)
        if next_interval > end_datetime:
            next_interval = end_datetime
        
        intervals.append((
            current.isoformat("T") + "Z",
            next_interval.isoformat("T") + "Z"
        ))
        current = next_interval
    
    return intervals


# =============================================================================
# DATA PERSISTENCE
# =============================================================================

def save_progress(video_data, species, country, output_dir='youtube_results_2016-now_fuzzymatch'):
    """
    Save video data to CSV, appending and deduplicating if file exists.
    
    The function ensures all required columns are present and deduplicates based
    on video_id to prevent duplicate entries when appending to existing files.
    
    :param video_data: List of video dictionaries with metadata
    :type video_data: list[dict]
    :param species: Scientific name of species
    :type species: str
    :param country: Country name
    :type country: str
    :param output_dir: Directory for output files, defaults to 'youtube_results_2016-now_fuzzymatch'
    :type output_dir: str, optional
    :return: None
    :rtype: None
    """
    if not video_data:
        return
    
    video_df = pd.DataFrame(video_data)
    
    # Ensure all required columns are present
    required_cols = [
        "video_id", "title", "description", "published_at", "channel_title",
        "url", "country", "species", "location", "radius", "published_after",
        "published_before", "total_results_estimate"
    ]
    video_df = video_df.reindex(columns=required_cols)
    
    # Create output directory
    os.makedirs(output_dir, exist_ok=True)
    
    # Generate filename
    safe_species = species.replace(' ', '_')
    safe_country = country.replace(' ', '_')
    file_name = f"{output_dir}/{safe_species}_{safe_country}.csv"
    
    # Append or create, then deduplicate by video_id
    if os.path.exists(file_name):
        existing_df = pd.read_csv(file_name)
        combined_df = pd.concat([existing_df, video_df], ignore_index=True)
        combined_df = combined_df.drop_duplicates(subset=['video_id']).reset_index(drop=True)
        combined_df.to_csv(file_name, index=False)
        print(f"  💾 Appended & deduplicated → {file_name}")
    else:
        video_df.to_csv(file_name, index=False)
        print(f"  💾 Saved → {file_name}")


# =============================================================================
# MAIN ORCHESTRATION FUNCTION
# =============================================================================

def get_video_data_for_species_and_countries(
    youtube, 
    species_list, 
    country_list, 
    published_after,
    published_before,
    progress_file,
    output_dir='youtube_results_2016-now_fuzzymatch'
):
    """
    Main orchestration function to fetch videos for all species-country combinations.
    
    This function manages the complete data collection workflow:
    
    1. Groups geographic circles by country name for aggregation
    2. Checks progress file to skip already-completed combinations
    3. For each species-country pair:
       - Queries all defined circles (geographic locations)
       - Detects when pagination limit prevents complete data capture
       - Automatically re-queries with time splitting when needed
       - Aggregates results from all circles for the country
       - Deduplicates and saves to CSV
       - Updates progress tracking
    4. Handles quota exhaustion gracefully, allowing script restart
    
    Pagination Verification:
    - Each fetch_videos() call iterates through ALL available pages
    - Uses nextPageToken until None (no more pages) or 500 results reached
    - If should_requery() detects missing videos, re-runs with time splits
    - Time splitting creates multiple smaller queries that each paginate fully
    
    :param youtube: Initialized YouTube Data API client
    :type youtube: googleapiclient.discovery.Resource
    :param species_list: List of species names to search
    :type species_list: list[str]
    :param country_list: List of dicts with 'country', 'location', 'radius' keys
    :type country_list: list[dict]
    :param published_after: ISO 8601 datetime string for search start
    :type published_after: str
    :param published_before: ISO 8601 datetime string for search end
    :type published_before: str
    :param progress_file: Path to progress tracking CSV
    :type progress_file: str
    :param output_dir: Directory for output files, defaults to 'youtube_results_2016-now_fuzzymatch'
    :type output_dir: str, optional
    :return: None (saves results to CSV files)
    :rtype: None
    """
    progress = load_progress(progress_file)
    
    # Group circles by country name for aggregation
    grouped_countries = {}
    for country_info in country_list:
        country_name = country_info['country']
        if country_name not in grouped_countries:
            grouped_countries[country_name] = []
        grouped_countries[country_name].append(country_info)
    
    # Temporary storage for current species-country results
    current_results = {}
    
    for species in species_list:
        print(f"\n{'='*60}")
        print(f"🔍 Searching for species: {species}")
        print(f"{'='*60}")
        
        for country_name, locations_info in grouped_countries.items():
            # Check if already processed
            if not progress.empty and \
               ((progress['species'] == species) & (progress['country'] == country_name)).any():
                print(f"  ⚡ Skipping {country_name} (already completed)")
                continue
            
            print(f"\n  📍 Processing country: {country_name}")
            print(f"     ({len(locations_info)} circle(s) to search)")
            
            # Initialize results for this species-country pair
            key = (species, country_name)
            current_results[key] = []
            
            # Process each circle (location) for this country
            for idx, country_info in enumerate(locations_info, 1):
                print(f"    Circle {idx}/{len(locations_info)}: "
                      f"Center={country_info['location']}, "
                      f"Radius={country_info['radius']}")
                
                # Fetch videos for this circle - FULLY PAGINATED
                # This call will iterate through ALL available pages until:
                # - nextPageToken is None (no more pages), OR
                # - 500 results are fetched (YouTube API limit)
                results, fetched_count, estimate = fetch_videos(
                    youtube, species, country_info, published_after, published_before
                )
                
                if results is None:
                    # Quota error - stop entire execution
                    print(f"\n⛔ Quota exhausted while processing {species} in {country_name}")
                    print(f"   Progress saved. Restart script to resume from this point.")
                    return
                
                current_results[key].extend(results)
                print(f"      → Fetched {fetched_count} videos (estimate: {estimate})")
                
                # Check if we should requery due to missing videos
                # This handles cases where the 500-result cap prevented full data capture
                if should_requery(fetched_count, estimate):
                    print(f"      ⚠️  Fetched {fetched_count}/{estimate} videos ({fetched_count/estimate*100:.1f}%)")
                    print(f"      🔄 Re-querying with time splitting to capture missing videos...")
                    
                    start_dt = datetime.strptime(published_after, "%Y-%m-%dT%H:%M:%SZ")
                    end_dt = datetime.strptime(published_before, "%Y-%m-%dT%H:%M:%SZ")
                    intervals = split_time_range(start_dt, end_dt, delta_days=100)
                    
                    # Each interval query will ALSO be fully paginated
                    for interval_idx, (pub_after, pub_before) in enumerate(intervals, 1):
                        print(f"        Interval {interval_idx}/{len(intervals)}: "
                              f"{pub_after[:10]} to {pub_before[:10]}")
                        
                        # This call also paginates fully through all available pages
                        sub_results, sub_count, sub_estimate = fetch_videos(
                            youtube, species, country_info, pub_after, pub_before
                        )
                        
                        if sub_results is None:
                            print(f"\n⛔ Quota exhausted during interval processing")
                            return
                        
                        current_results[key].extend(sub_results)
                        print(f"          → Fetched {sub_count} videos (estimate: {sub_estimate})")
                        time.sleep(1)
                
                time.sleep(1)  # Delay between circles
            
            # Save aggregated results for this country
            final_results = current_results[key]
            print(f"\n  📊 Total unique videos for {country_name}: "
                  f"{len(set(v['video_id'] for v in final_results))}")
            
            save_progress(final_results, species, country_name, output_dir)
            update_progress(species, country_name, progress_file)
            
            time.sleep(1)  # Delay between countries


# =============================================================================
# CONFIGURATION DATA
# =============================================================================

def get_default_date_range():
    """
    Return default date range for video searches.
    
    :return: Tuple of (published_after, published_before) as ISO 8601 strings
    :rtype: tuple(str, str)
    """
    return ("2016-01-01T00:00:00Z", "2025-07-15T00:00:00Z")


def get_eu_countries():
    """
    Return list of EU countries with multiple circles for comprehensive coverage.
    
    Large or irregularly-shaped countries are covered by multiple overlapping
    circles to ensure complete geographic coverage.
    
    :return: List of dicts with 'country', 'location' (lat,lon), 'radius' keys
    :rtype: list[dict]
    """
    return [
        {"country": "Austria",        "location": "47.5162,14.5501",   "radius": "180km"},
        {"country": "Belgium",        "location": "50.5039,4.4699",    "radius": "120km"},
        {"country": "Bulgaria",       "location": "42.7339,25.4858",   "radius": "180km"},
        {"country": "Croatia",        "location": "45.1000,15.2000",   "radius": "200km"},
        {"country": "Croatia",        "location": "43.5000,16.5000",   "radius": "100km"},
        {"country": "Cyprus",         "location": "35.1264,33.4299",   "radius": "70km"},
        {"country": "Czechia",        "location": "49.8175,15.4730",   "radius": "150km"},
        {"country": "Denmark",        "location": "56.2639,9.5018",    "radius": "180km"},
        {"country": "Denmark",        "location": "55.0000,10.0000",   "radius": "80km"},
        {"country": "Estonia",        "location": "58.5953,25.0136",   "radius": "150km"},
        {"country": "Finland",        "location": "61.9241,25.7482",   "radius": "350km"},
        {"country": "Finland",        "location": "67.0000,26.0000",   "radius": "200km"},
        {"country": "France",         "location": "46.6034,1.8883",    "radius": "350km"},
        {"country": "France",         "location": "48.5000,6.0000",    "radius": "150km"},
        {"country": "France",         "location": "43.5000,-1.0000",   "radius": "150km"},
        {"country": "France",         "location": "42.0000,9.0000",    "radius": "70km"},
        {"country": "Germany",        "location": "51.1657,10.4515",   "radius": "300km"},
        {"country": "Germany",        "location": "53.5000,9.5000",    "radius": "150km"},
        {"country": "Germany",        "location": "48.5000,11.5000",   "radius": "150km"},
        {"country": "Greece",         "location": "39.0742,21.8243",   "radius": "250km"},
        {"country": "Greece",         "location": "35.5000,24.0000",   "radius": "150km"},
        {"country": "Greece",         "location": "37.5000,26.0000",   "radius": "150km"},
        {"country": "Hungary",        "location": "47.1625,19.5033",   "radius": "150km"},
        {"country": "Ireland",        "location": "53.1424,-7.6921",   "radius": "150km"},
        {"country": "Italy",          "location": "41.8719,12.5674",   "radius": "300km"},
        {"country": "Italy",          "location": "45.5000,10.5000",   "radius": "150km"},
        {"country": "Italy",          "location": "38.5000,16.0000",   "radius": "150km"},
        {"country": "Italy",          "location": "40.0000,9.0000",    "radius": "120km"},
        {"country": "Latvia",         "location": "56.8796,24.6032",   "radius": "150km"},
        {"country": "Lithuania",      "location": "55.1694,23.8813",   "radius": "150km"},
        {"country": "Luxembourg",     "location": "49.8153,6.1296",    "radius": "50km"},
        {"country": "Malta",          "location": "35.9375,14.3754",   "radius": "20km"},
        {"country": "Netherlands",    "location": "52.1326,5.2913",    "radius": "100km"},
        {"country": "Poland",         "location": "51.9194,19.1451",   "radius": "250km"},
        {"country": "Poland",         "location": "53.5000,22.0000",   "radius": "150km"},
        {"country": "Portugal",       "location": "39.3999,-8.2245",   "radius": "200km"},
        {"country": "Portugal",       "location": "38.7000,-28.0000",  "radius": "150km"},
        {"country": "Portugal",       "location": "32.7000,-17.0000",  "radius": "100km"},
        {"country": "Romania",        "location": "45.9432,24.9668",   "radius": "250km"},
        {"country": "Romania",        "location": "46.0000,28.0000",   "radius": "100km"},
        {"country": "Slovakia",       "location": "48.6690,19.6990",   "radius": "150km"},
        {"country": "Slovenia",       "location": "46.1512,14.9955",   "radius": "100km"},
        {"country": "Spain",          "location": "40.4637,-3.7492",   "radius": "400km"},
        {"country": "Spain",          "location": "42.5000,-7.5000",   "radius": "150km"},
        {"country": "Spain",          "location": "37.0000,-4.0000",   "radius": "150km"},
        {"country": "Spain",          "location": "28.5000,-16.0000",  "radius": "150km"},
        {"country": "Spain",          "location": "39.7000,2.9000",    "radius": "100km"},
        {"country": "Sweden",         "location": "60.1282,18.6435",   "radius": "400km"},
        {"country": "Sweden",         "location": "65.0000,18.0000",   "radius": "300km"},
        {"country": "Sweden",         "location": "56.0000,14.0000",   "radius": "150km"},
        {"country": "United Kingdom", "location": "54.0000,-2.5000",   "radius": "300km"},
        {"country": "United Kingdom", "location": "51.0000,0.0000",    "radius": "150km"},
        {"country": "United Kingdom", "location": "57.0000,-4.5000",   "radius": "200km"},
        {"country": "United Kingdom", "location": "54.5000,-7.0000",   "radius": "80km"},
        {"country": "Switzerland",    "location": "46.8182,8.2275",    "radius": "100km"},
        {"country": "Norway",         "location": "60.4720,8.4689",    "radius": "400km"},
        {"country": "Norway",         "location": "67.0000,14.0000",   "radius": "300km"},
        {"country": "Norway",         "location": "70.0000,24.0000",   "radius": "200km"},
        {"country": "Norway",         "location": "59.0000,10.0000",   "radius": "100km"}
    ]


# =============================================================================
# MAIN SCRIPT EXECUTION
# =============================================================================

def main():
    """
    Main entry point when running as a standalone script.
    
    Loads configuration, initializes API client, and runs the full data
    collection pipeline for all species and countries.
    
    Environment Variables Required:
    - YT_API_KEY: YouTube Data API v3 key
    
    Files Required:
    - list_of_union_concern.csv: Must contain 'Scientific Name' column
    - .env: Must contain YT_API_KEY=your_key_here
    
    :return: None
    :rtype: None
    """
    # Load environment variables
    load_dotenv()
    api_key = os.getenv("YT_API_KEY")
    
    if not api_key:
        print("Error: YT_API_KEY not found in environment variables.")
        print("Please create a .env file with: YT_API_KEY=your_key_here")
        return
    
    # Initialize YouTube API client
    youtube = build('youtube', 'v3', developerKey=api_key)
    
    # Configuration
    progress_file = 'progress_yt_2016-now_fuzzymatch.csv'
    output_dir = 'youtube_results_2016-now_fuzzymatch'
    published_after, published_before = get_default_date_range()
    
    # Load species list
    try:
        species_df = pd.read_csv('list_of_union_concern.csv')
        species_list = species_df['Scientific Name'].dropna().unique().tolist()
        print(f"✓ Loaded {len(species_list)} species from list_of_union_concern.csv")
    except FileNotFoundError:
        print("Error: 'list_of_union_concern.csv' not found.")
        print("Please ensure this file exists in the current directory.")
        return
    
    # Get country circles
    countries = get_eu_countries()
    print(f"✓ Loaded {len(countries)} geographic circles covering EU countries")
    
    # Run data collection
    print(f"\n{'='*60}")
    print(f"Starting YouTube video collection")
    print(f"Date range: {published_after[:10]} to {published_before[:10]}")
    print(f"Progress file: {progress_file}")
    print(f"Output directory: {output_dir}")
    print(f"{'='*60}\n")
    
    get_video_data_for_species_and_countries(
        youtube, 
        species_list, 
        countries, 
        published_after,
        published_before,
        progress_file,
        output_dir
    )
    
    print(f"\n{'='*60}")