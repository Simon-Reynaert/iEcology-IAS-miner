#load dependencies
import pandas as pd
import requests
import time
from datetime import datetime, timedelta
import os
import pytz
from dotenv import load_dotenv

#Load environment variables from a .env file for user agent identification
load_dotenv()
USER_AGENT = os.getenv("WIKI_USER_AGENT")

# Load the Wikipedia sitelinks DataFrame
sitelinks_df = pd.read_csv('species_wikipedia_sitelinks.csv')

# Wikimedia API base URL for per-article pageviews
BASE_URL = "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article"

# Custom User-Agent string
HEADERS = {
    "User-Agent": USER_AGENT
}

# Define date range from January 2016-01-01 until yesterday (since today is not complete and will give zero pageviews wrongly)
utc = pytz.UTC
today = datetime.now(utc).date()
yesterday = today - timedelta(days=1)
start_date = datetime(2016, 1, 1).strftime("%Y%m%d")
end_date = yesterday.strftime("%Y%m%d")  # Fetch up to yesterday

def fetch_daily_pageviews(language, title, existing_dates=None, start_fetch_date=None):
    project = f"{language}.wikipedia"
    if start_fetch_date:
        url = f"{BASE_URL}/{project}/all-access/user/{title}/daily/{start_fetch_date}/{end_date}"
    else:
        url = f"{BASE_URL}/{project}/all-access/user/{title}/daily/{start_date}/{end_date}"
    try:
        response = requests.get(url, headers=HEADERS)
        response.raise_for_status()
        data = response.json()
        if existing_dates:
            return {item["timestamp"][:8]: item["views"] for item in data.get("items", []) if item["timestamp"][:8] not in existing_dates}
        else:
            return {item["timestamp"][:8]: item["views"] for item in data.get("items", [])}
    except requests.exceptions.HTTPError as e:
        if e.response.status_code == 404:
            print(f"Error fetching pageviews for {language}:{title} - 404 Not Found")
            return {}
        else:
            print(f"Error fetching pageviews for {language}:{title} - {e}")
            return {}
    except Exception as e:
        print(f"Error fetching pageviews for {language}:{title} - {e}")
        return {}

def load_existing_data(file_path):
    if os.path.exists(file_path):
        return pd.read_csv(file_path)
    else:
        return pd.DataFrame()

def get_missing_dates(existing_df, date_columns):
    if existing_df.empty:
        return date_columns
    existing_dates = existing_df.columns[3:].tolist()
    return [date for date in date_columns if date not in existing_dates]

# Load existing data if it exists
output_file = 'species_pageviews_analysis_2016_present.csv'
existing_data = load_existing_data(output_file)

# Generate date columns
date_columns = pd.date_range(start="2016-01-01", end=yesterday).strftime("%Y%m%d").tolist()

# Get missing dates
missing_dates = get_missing_dates(existing_data, date_columns)

if not missing_dates:
    print("Data is up to date. No new data to fetch.")
else:
    species_pageviews = []
    start_time = time.time()

    for _, row in sitelinks_df.iterrows():
        language = row['Language']
        title = row['Wikipedia Title'].replace(' ', '_')
        scientific_name = row['Scientific Name']

        if existing_data.empty or scientific_name not in existing_data['Scientific Name'].values or language not in existing_data['Language'].values:
            print(f"Fetching data for {scientific_name} ({language}) from {start_date} to {end_date}")
            daily_pageviews = fetch_daily_pageviews(language, title)
            pageviews_list = [daily_pageviews.get(date, '') for date in date_columns] #adapt '' to 0 if you want to auto fill zeroes
        else:
            existing_row = existing_data[(existing_data['Scientific Name'] == scientific_name) & (existing_data['Language'] == language)].iloc[0]
            existing_dates = existing_row.index[3:].tolist()
            # Ensure yesterday's data is always fetched
            if yesterday.strftime("%Y%m%d") in existing_dates:
                existing_dates.remove(yesterday.strftime("%Y%m%d"))  # Remove yesterday from existing to refetch
            missing_dates_for_species = [date for date in date_columns if date not in existing_dates]
            if missing_dates_for_species:
                first_missing_date = min(missing_dates_for_species) #get the earliest missing date.
                print(f"Fetching data for {scientific_name} ({language}) from {first_missing_date} to {end_date}")
                daily_pageviews = fetch_daily_pageviews(language, title, existing_dates, first_missing_date)
            else:
                daily_pageviews = {}
            pageviews_list = [existing_row.get(date, '') if date in existing_dates else daily_pageviews.get(date, '') for date in date_columns] #adapt '' to 0 if you want to auto fill zeroes

        species_data = {"Scientific Name": scientific_name, "Language": language, "Wikipedia Title": title}
        species_data.update(dict(zip(date_columns, pageviews_list)))

        species_pageviews.append(species_data)
        time.sleep(1)

    # Create DataFrame and save to CSV
    new_data = pd.DataFrame(species_pageviews)

    if existing_data.empty:
        combined_data = new_data
    else:
        combined_data = pd.concat([existing_data[['Scientific Name', 'Language', 'Wikipedia Title']], new_data.drop(columns=['Scientific Name', 'Language', 'Wikipedia Title'])], axis=1)

    combined_data.to_csv(output_file, index=False)

    end_time = time.time()
    print(f"Execution time: {end_time - start_time:.2f} seconds")

    # Display sample output
    print(combined_data.head())