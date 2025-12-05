import pandas as pd
import requests
import time
import os
from datetime import datetime, timedelta
import pytz
from dotenv import load_dotenv

# --- CONSTANTS (Intended to never change) ---

# Base URL for the Wikimedia Pageviews API
BASE_API_URL = "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article"

# Timezone for date handling
TIMEZONE = pytz.UTC

# --- CONFIGURATION VARIABLES (Can be changed/defaulted) ---

# Default API wait time in seconds to prevent rate limiting
api_wait_time_seconds = 1


def get_wiki_api_headers(user_agent: str) -> dict:
    """Returns the headers for Wikimedia API requests, requiring a User-Agent string."""
    return {"User-Agent": user_agent}

def fetch_daily_pageviews(language: str, title: str, start_date: str, end_date: str, headers: dict) -> pd.DataFrame:
    """
    Fetches pageviews for a single Wikipedia article over a date range from the Wikimedia API.

    Args:
        language (str): The language code (e.g., 'en').
        title (str): The Wikipedia article title.
        start_date (str): The inclusive start date in YYYYMMDD format.
        end_date (str): The exclusive end date in YYYYMMDD format (data fetched up to day before).
        headers (dict): Request headers, including a User-Agent.

    Returns:
        A DataFrame containing 'timestamp' and 'views' for the fetched data, or an empty DataFrame on error.
    """
    project = f"{language}.wikipedia"
    url = f"{BASE_API_URL}/{project}/all-access/user/{title}/daily/{start_date}/{end_date}"

    try:
        response = requests.get(url, headers=headers)
        response.raise_for_status()
        data = response.json()
        if "items" in data:
            return pd.DataFrame(data["items"])
        return pd.DataFrame()
    except requests.exceptions.HTTPError as e:
        if e.response.status_code == 404:
            print(f"404 Not Found: pageviews for {language}:{title} not available.")
        else:
            print(f"HTTP Error: {e} for {language}:{title}. Skipping.")
        return pd.DataFrame()
    except Exception as e:
        print(f"Unexpected error fetching pageviews for {language}:{title}: {e}. Skipping.")
        return pd.DataFrame()

def _load_existing_data(file_path: str) -> pd.DataFrame:
    """Helper function to safely load existing pageview data from a CSV file."""
    if os.path.exists(file_path):
        try:
            return pd.read_csv(file_path)
        except Exception as e:
            print(f"Error loading existing data from {file_path}: {e}")
            return pd.DataFrame()
    return pd.DataFrame()


# --- PUBLIC INTERFACE FUNCTION ---

def run_lang_pageviews_fetcher(
    start_date: str,
    end_date: str,
    input_file: str,
    output_file: str,
):
    """
    The main callable function to orchestrate fetching and processing pageviews for a custom date range.

    Args:
        start_date (str): The inclusive start date for fetching (YYYY-MM-DD).
        end_date (str): The exclusive end date for fetching (YYYY-MM-DD). Data is fetched up to the day before.
        input_file (str): Path to the CSV file containing species sitelinks.
        output_file (str): Path to the CSV file to save/update the pageview data.
    """
    print(f"Configuration: Fetching data from {start_date} up to (but not including) {end_date}")

    # Load environment variables for user agent identification
    load_dotenv()
    user_agent = os.getenv("WIKI_USER_AGENT")
    if not user_agent:
        print("Warning: WIKI_USER_AGENT not set. Add it to your .env file.")
        return

    # Load the Wikipedia sitelinks DataFrame
    try:
        sitelinks_df = pd.read_csv(input_file)
        # Standardize column names (assuming columns like 'Wikipedia Title' and 'Scientific Name')
        sitelinks_df.columns = sitelinks_df.columns.str.replace(' ', '_').str.strip()
    except FileNotFoundError:
        print(f"Sitelinks file '{input_file}' not found.")
        return
    except KeyError:
        print("Error: Sitelinks file is missing required columns (e.g., 'Language', 'Wikipedia_Title', 'Scientific_Name').")
        return

    headers = get_wiki_api_headers(user_agent)

    # Convert user-supplied dates to YYYYMMDD format for the API
    try:
        start_date_api = datetime.strptime(start_date, "%Y-%m-%d").strftime("%Y%m%d")
        end_date_api = datetime.strptime(end_date, "%Y-%m-%d").strftime("%Y%m%d")
        
        # Calculate the actual last date fetched for DataFrame column creation
        # (This is the day *before* the exclusive end_date)
        end_fetch_date = datetime.strptime(end_date, "%Y-%m-%d").date() - timedelta(days=1)
        
    except ValueError:
        print("Error: Invalid date format. Use YYYY-MM-DD.")
        return

    # Generate the list of all date columns (YYYYMMDD) for the full range required for the output file
    all_dates = pd.date_range(start=start_date, end=end_fetch_date).strftime("%Y%m%d").tolist()
    
    if not all_dates:
        print(f"Date range is empty between {start_date} and {end_date} (exclusive). Exiting.")
        return
    
    # Load existing data to handle updates
    existing_df = _load_existing_data(output_file)

    # Initialize the output DataFrame if it's empty
    if existing_df.empty:
        print(f"Creating new output file: {output_file}")
        # Create a DataFrame of zeros for all dates
        date_df = pd.DataFrame(0, index=sitelinks_df.index, columns=all_dates)
        existing_df = pd.concat([sitelinks_df, date_df], axis=1)
        existing_df['Scientific_Name'] = existing_df['Scientific_Name'].astype(str)
        existing_df['Language'] = existing_df['Language'].astype(str)

    print("Starting pageview data fetch...")
    for _, row in sitelinks_df.iterrows():
        language = row['Language']
        title = row['Wikipedia_Title'].replace(' ', '_')
        scientific_name = row['Scientific_Name']

        # Determine the correct start date for fetching (resume logic)
        existing_row = existing_df[
            (existing_df['Scientific_Name'] == scientific_name) & 
            (existing_df['Language'] == language)
        ]
        
        start_api_resume = start_date_api

        if not existing_row.empty:
            existing_row_values = existing_row.iloc[0].to_dict()
            existing_date_cols = [col for col in all_dates if col in existing_row_values]
            
            # Find the last date with a non-zero view count
            valid_dates = [d for d in existing_date_cols if existing_row_values.get(d) is not None and existing_row_values.get(d) > 0]
            
            if valid_dates:
                last_date_str = max(valid_dates)
                last_date = datetime.strptime(last_date_str, "%Y%m%d").date()
                
                if last_date >= end_fetch_date:
                    print(f"Data for {scientific_name} ({language}) is up-to-date within range. Skipping.")
                    continue
                
                # Resume fetching from the day after the last valid data point
                fetch_from = last_date + timedelta(days=1)
                start_api_resume = fetch_from.strftime("%Y%m%d")

        print(f"Fetching {scientific_name} ({language}) from {start_api_resume} to {end_date_api} (exclusive)...")
        
        daily_df = fetch_daily_pageviews(language, title, start_api_resume, end_date_api, headers)

        if not daily_df.empty:
            # Data transformation
            daily_df["timestamp"] = pd.to_datetime(daily_df["timestamp"], format="%Y%m%d%H").dt.strftime("%Y%m%d")
            daily_df = daily_df.groupby('timestamp')['views'].sum().reset_index()
            daily_df = daily_df.rename(columns={'timestamp': 'date'})
            
            # Update the main DataFrame
            daily_df_pivot = daily_df.pivot_table(index=[], columns='date', values='views')
            target_index = existing_df[
                (existing_df['Scientific_Name'] == scientific_name) & 
                (existing_df['Language'] == language)
            ].index
            
            if not target_index.empty:
                daily_df_pivot.index = target_index
                existing_df.update(daily_df_pivot.fillna(0))

        # Save the updated DataFrame after each species is processed
        existing_df.to_csv(output_file, index=False)
        print(f"Saved data for {scientific_name} ({language}) to {output_file}")
        
        # Pause to avoid hammering the API
        time.sleep(api_wait_time_seconds)

    print("\n✅ All available data processed for the custom date range.")


# --- STANDALONE EXECUTION BLOCK ---

def main():
    """
    Main execution function for the Wikipedia Pageviews script (Standalone Mode).
    Uses defined default configuration.
    """
    # --- DEFAULT CONFIGURATION FOR STANDALONE RUN ---
    
    # Standard file names often used for input/output.
    DEFAULT_INPUT_FILE = "unionconcern_invasive_species_wikipedia_links_2025.csv"
    DEFAULT_OUTPUT_FILE = "species_pageviews_analysis_2017-today.csv"
    
    # Wikimedia Pageviews API started capturing data consistently around 2015/2016.
    # We choose 2017-01-01 for a safer, long-term default historical start date.
    DEFAULT_START_DATE = "2017-01-01" 
    
    # The default end date is set to the start of the current day (yesterday's data is the latest full day available).
    # Since the API end date is EXCLUSIVE, we use today's date to fetch data up to yesterday.
    today = datetime.now(TIMEZONE).date().strftime("%Y-%m-%d")
    DEFAULT_END_DATE = today 

    print("\n--- Running Wikipedia Pageviews Fetcher (Standalone Mode) ---")
    
    # Call the core function with default parameters
    run_lang_pageviews_fetcher(
        start_date=DEFAULT_START_DATE,
        end_date=DEFAULT_END_DATE,
        input_file=DEFAULT_INPUT_FILE,
        output_file=DEFAULT_OUTPUT_FILE
    )

if __name__ == "__main__":
    main()