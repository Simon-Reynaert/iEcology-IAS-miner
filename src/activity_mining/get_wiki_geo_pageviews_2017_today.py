import os
import time
import random
import requests
import pandas as pd
from datetime import datetime, timedelta
from io import StringIO
from dotenv import load_dotenv

# --- Configuration for URL Selection ---
# The date that separates the two different datasets (historical vs. current).
DATE_CUTOFF_NEW_DATASET = datetime(2023, 2, 6)

# Base URLs for the two Wikimedia datasets
URL_HISTORICAL = "https://analytics.wikimedia.org/published/datasets/country_project_page_historical"
URL_CURRENT = "https://analytics.wikimedia.org/published/datasets/country_project_page"

def get_wikimedia_url(date_str: str) -> str:
    """
    Selects the correct Wikimedia base URL based on the date.
    Dates < 2023-02-06 use historical data; 
    Dates >= 2023-02-06 use current data.

    Args:
        date_str (str): Date in YYYY-MM-DD format.

    Returns:
        str: The correct base URL for the given date.
    """
    date_obj = datetime.strptime(date_str, "%Y-%m-%d")
    if date_obj < DATE_CUTOFF_NEW_DATASET:
        return URL_HISTORICAL
    else:
        return URL_CURRENT

def fetch_and_process_pageviews(
    input_file: str,
    output_file: str,
    start_date_str: str,
    end_date_str: str,
    user_agent: str
):
    """
    Fetch and save Wikimedia species pageviews for a range of dates,
    automatically selecting the correct data URL.

    (Core logic: Load species, generate dates, check existing data,
     loop through dates, fetch, filter, aggregate, and save.)

    Args:
        input_file (str): Path to CSV file with columns 'Scientific Name' and 'Wikidata Q-number'.
        output_file (str): Path to CSV file where results will be saved.
        start_date_str (str): Start date in YYYY-MM-DD format.
        end_date_str (str): End date in YYYY-MM-DD format.
        user_agent (str): User-Agent string for Wikimedia requests.

    Returns:
        None. Writes data to `output_file`.
    """
    # --- 1. Load species CSV and map Q-number -> species name ---
    try:
        species_df = pd.read_csv(input_file)
        q_to_name = dict(zip(species_df["Wikidata Q-number"].astype(str), species_df["Scientific Name"]))
    except FileNotFoundError:
        print(f"Species file '{input_file}' not found.")
        return
    if not q_to_name:
        print("Species file empty or missing required columns.")
        return

    # --- 2. Generate list of all dates in the requested range ---
    try:
        start_date = datetime.strptime(start_date_str, "%Y-%m-%d")
        end_date = datetime.strptime(end_date_str, "%Y-%m-%d")
    except ValueError:
        print("Error: Date arguments must be in YYYY-MM-DD format.")
        return

    if start_date > end_date:
        print("Error: Start date cannot be after end date.")
        return

    all_dates = [(start_date + timedelta(days=i)).strftime("%Y-%m-%d")
                 for i in range((end_date - start_date).days + 1)]

    # --- 3. Load existing CSV if it exists and find missing dates ---
    if os.path.exists(output_file):
        wide_df = pd.read_csv(output_file)
        existing_date_cols = [c for c in wide_df.columns if c not in ["Scientific Name", "Country", "Wikidata Q-number"]]
    else:
        wide_df = pd.DataFrame(columns=["Scientific Name", "Country", "Wikidata Q-number"])
        existing_date_cols = []

    missing_dates = [d for d in all_dates if d not in existing_date_cols]
    if not missing_dates:
        print("All dates already present in the output file. Nothing to fetch.")
        return

    print(f"{len(missing_dates)} missing dates to fetch, ranging from {missing_dates[0]} to {missing_dates[-1]}.")

    # --- 4. Setup headers ---
    headers = {"User-Agent": user_agent}

    # --- 5. Process each missing date ---
    for date_str in missing_dates:
        # **Dynamic URL Selection**
        base_url = get_wikimedia_url(date_str)
        url = f"{base_url}/{date_str}.tsv"
        print(f"Fetching data for {date_str} from {base_url.split('/')[-1]}...")

        try:
            # Fetch daily TSV
            resp = requests.get(url, headers=headers)
            resp.raise_for_status() 

            # Parse TSV into DataFrame
            day_df = pd.read_csv(
                StringIO(resp.text),
                sep="\t",
                names=["Country", "Country Code", "Project", "Wiki Page ID", "Article Title",
                       "Wikidata Q-number", "Pageviews"],
                header=0
            )

            # Filter, aggregate, and merge
            day_df["Wikidata Q-number"] = day_df["Wikidata Q-number"].astype(str)
            day_df["Pageviews"] = pd.to_numeric(day_df["Pageviews"], errors="coerce").fillna(0)
            day_filtered = day_df[day_df["Wikidata Q-number"].isin(q_to_name.keys())]
            day_grouped = day_filtered.groupby(["Country", "Wikidata Q-number"])["Pageviews"].sum().reset_index()

            if not day_grouped.empty:
                day_grouped["Scientific Name"] = day_grouped["Wikidata Q-number"].map(q_to_name)
                day_data = day_grouped[["Scientific Name", "Country", "Wikidata Q-number", "Pageviews"]]
                day_data = day_data.rename(columns={"Pageviews": date_str})

                wide_df = pd.merge(
                    wide_df,
                    day_data,
                    on=["Scientific Name", "Country", "Wikidata Q-number"],
                    how="outer"
                )

                date_cols = [c for c in wide_df.columns if c not in ["Scientific Name", "Country", "Wikidata Q-number"]]
                wide_df[date_cols] = wide_df[date_cols].fillna(0)

                # Save after each date (periodic saving)
                wide_df.to_csv(output_file, index=False)
                print(f"Saved data for {date_str} to {output_file}")
            else:
                print(f"No relevant data for {date_str}, skipping.")

        except requests.exceptions.RequestException as e:
            if '404' in str(e):
                 print(f"File not found (404) for {date_str} at {url}. Skipping.")
            else:
                print(f"Error fetching {date_str}: {e}. Skipping.")
        except pd.errors.EmptyDataError:
            print(f"Empty data or invalid TSV format for {date_str}, skipping.")
        except Exception as e:
            print(f"Unexpected error for {date_str}: {e}. Skipping.")

        # Polite delay
        time.sleep(random.uniform(1, 3))

    print(f"✅ All requested dates processed. Final CSV at {output_file}.")

# --- Main Entry Function for Both Notebooks and Standalone Execution ---

def run_pageviews_fetcher(
    start_date: str,
    end_date: str,
    input_file: str = "unionconcern_invasive_species_qnumbers_2025.csv",
    output_file: str = "species_pageviews_wiki_geolocated_combined.csv"
):
    """
    Main entry point for the script. Loads WIKI_USER_AGENT from .env and 
    executes the pageview fetching process for the specified date range.

    Args:
        start_date (str): Start date in YYYY-MM-DD format.
        end_date (str): End date in YYYY-MM-DD format.
        input_file (str): Path to species Q-number CSV (default: 'unionconcern_invasive_species_qnumbers_2025.csv').
        output_file (str): Path to the output CSV file (default: 'species_pageviews_wiki_geolocated_combined.csv').
    """
    # Load Environment Variables
    load_dotenv()
    WIKI_USER_AGENT = os.getenv("WIKI_USER_AGENT")

    if not WIKI_USER_AGENT:
        print("❌ Warning: WIKI_USER_AGENT not set. Please add it to your .env file.")
        return

    print("--- Starting Wikimedia Pageviews Fetcher ---")
    print(f"Requested Date Range: {start_date} to {end_date}")
    print(f"Output File: {output_file}")
    print(f"Input File: {input_file}")
    print("-" * 40)

    fetch_and_process_pageviews(
        input_file=input_file,
        output_file=output_file,
        start_date_str=start_date,
        end_date_str=end_date,
        user_agent=WIKI_USER_AGENT
    )

# --- Standalone Execution Block ---

if __name__ == "__main__":
    # Default configuration for standalone execution
    DEFAULT_START_DATE = "2017-02-09" # First date available for geo-located data
    # Set default end date to yesterday to ensure data is likely published
    DEFAULT_END_DATE = (datetime.now() - timedelta(days=1)).strftime("%Y-%m-%d")
    
    # Run the function with default arguments
    run_pageviews_fetcher(
        start_date=DEFAULT_START_DATE,
        end_date=DEFAULT_END_DATE,
        input_file="unionconcern_invasive_species_qnumbers_2025.csv",
        output_file="species_pageviews_wiki_geolocated_combined_default.csv"
    )