import pandas as pd
import requests
import time
from datetime import datetime, timedelta
import os
import pytz
from dotenv import load_dotenv

def get_wiki_api_headers(user_agent: str) -> dict:
    """Returns the headers for Wikimedia API requests."""
    return {"User-Agent": user_agent}

def fetch_daily_pageviews(language: str, title: str, start_date: str, end_date: str, headers: dict) -> pd.DataFrame:
    """
    Fetches pageviews for a single Wikipedia article over a date range.

    Args:
        language (str): The language code (e.g., 'en').
        title (str): The Wikipedia article title.
        start_date (str): The start date in YYYYMMDD format.
        end_date (str): The end date in YYYYMMDD format.
        headers (dict): Request headers, including a User-Agent.

    Returns:
        A DataFrame containing 'timestamp' and 'views' for the fetched data.
    """
    project = f"{language}.wikipedia"
    url = f"https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/{project}/all-access/user/{title}/daily/{start_date}/{end_date}"

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

def load_existing_data(file_path: str) -> pd.DataFrame:
    """Loads existing data from a CSV file if it exists."""
    if os.path.exists(file_path):
        return pd.read_csv(file_path)
    return pd.DataFrame()

def fetch_and_process_pageviews(
    sitelinks_file: str,
    output_file: str,
    start_date_str: str,
    end_date_str: str
):
    """
    Main function to orchestrate fetching and processing pageviews.
    """
    # Load environment variables for user agent identification
    load_dotenv()
    user_agent = os.getenv("WIKI_USER_AGENT")
    if not user_agent:
        print("Warning: WIKI_USER_AGENT not set. Add it to your .env file.")
        return

    # Load the Wikipedia sitelinks DataFrame
    try:
        sitelinks_df = pd.read_csv(sitelinks_file)
    except FileNotFoundError:
        print(f"Sitelinks file '{sitelinks_file}' not found.")
        return

    headers = get_wiki_api_headers(user_agent)

    # Convert date strings to datetime objects and define fetch range
    utc = pytz.UTC
    yesterday = datetime.now(utc).date() - timedelta(days=1)
    end_fetch_date = yesterday

    # Generate the list of all date columns
    all_dates = pd.date_range(start=start_date_str, end=end_fetch_date).strftime("%Y%m%d").tolist()
    
    # Load existing data to handle updates
    existing_df = load_existing_data(output_file)

    # If existing data does not exist, create a new DataFrame with all dates efficiently.
    if existing_df.empty:
        # Create a DataFrame of zeros with the same index as sitelinks_df and columns for all dates
        date_df = pd.DataFrame(0, index=sitelinks_df.index, columns=all_dates)
        # Concatenate the sitelinks and date DataFrames horizontally in a single, efficient operation
        existing_df = pd.concat([sitelinks_df, date_df], axis=1)

    print("Starting pageview data fetch...")
    for _, row in sitelinks_df.iterrows():
        language = row['Language']
        title = row['Wikipedia Title'].replace(' ', '_')
        scientific_name = row['Scientific Name']

        # Determine the correct start date for fetching by checking the last valid date
        # in the existing DataFrame for this species-language pair.
        existing_row = existing_df[(existing_df['Scientific Name'] == scientific_name) & (existing_df['Language'] == language)]
        if not existing_row.empty:
            existing_row_values = existing_row.iloc[0].to_dict()
            # Find the last date with a non-zero value to resume from
            existing_date_cols = [col for col in existing_row_values if col.isdigit()]
            valid_dates = [d for d in existing_date_cols if existing_row_values.get(d) is not None and existing_row_values.get(d) > 0]
            if valid_dates:
                last_date_str = max(valid_dates)
                last_date = datetime.strptime(last_date_str, "%Y%m%d").date()
                if last_date >= end_fetch_date:
                    print(f"Data for {scientific_name} ({language}) is up-to-date. Skipping.")
                    continue
                fetch_from = last_date + timedelta(days=1)
                start_api_date = fetch_from.strftime("%Y%m%d")
            else:
                start_api_date = datetime.strptime(start_date_str, "%Y-%m-%d").strftime("%Y%m%d")
        else:
            # This case should not be reached with the new initialization logic, but kept for robustness.
            start_api_date = datetime.strptime(start_date_str, "%Y-%m-%d").strftime("%Y%m%d")

        print(f"Fetching {scientific_name} ({language}) from {start_api_date} to {yesterday.strftime('%Y%m%d')}...")
        
        daily_df = fetch_daily_pageviews(language, title, start_api_date, yesterday.strftime("%Y%m%d"), headers)

        if not daily_df.empty:
            daily_df["timestamp"] = pd.to_datetime(daily_df["timestamp"], format="%Y%m%d%H").dt.strftime("%Y%m%d")
            
            # Aggregate views by date for articles with multiple daily entries
            daily_df = daily_df.groupby('timestamp')['views'].sum().reset_index()
            daily_df = daily_df.rename(columns={'timestamp': 'date'})
            
            # Use `set_index` and `update` to fill the master DataFrame efficiently
            daily_df_pivot = daily_df.pivot_table(index=[], columns='date', values='views')
            daily_df_pivot.index = existing_df[(existing_df['Scientific Name'] == scientific_name) & (existing_df['Language'] == language)].index
            
            existing_df.update(daily_df_pivot)

        # Save the updated DataFrame after each species is processed
        existing_df.to_csv(output_file, index=False)
        print(f"Saved data for {scientific_name} ({language}) to {output_file}")
        
        # Pause to avoid hammering the API
        time.sleep(1)

    print("All available data processed.")

# Main function to run the script
def main():
    """Main execution function for the Wikipedia Pageviews script."""
    # Configuration
    SITELINKS_FILE = "species_wikipedia_sitelinks.csv"
    OUTPUT_FILE = "species_pageviews_analysis_2016_present.csv"
    START_DATE = "2016-01-01"
    END_DATE = datetime.now(pytz.UTC).date().strftime("%Y-%m-%d")

    fetch_and_process_pageviews(
        sitelinks_file=SITELINKS_FILE,
        output_file=OUTPUT_FILE,
        start_date_str=START_DATE,
        end_date_str=END_DATE
    )

if __name__ == "__main__":
    main()
