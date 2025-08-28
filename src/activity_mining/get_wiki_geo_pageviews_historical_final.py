# get_wiki_geo_pageviews_2017-2023_final.py

import os
import time
import random
import requests
import pandas as pd
from datetime import datetime, timedelta
from io import StringIO
from dotenv import load_dotenv

def fetch_and_process_pageviews(
    species_file: str,
    output_file: str,
    start_date_str: str,
    end_date_str: str,
    user_agent: str
):
    """
    Fetch and save Wikimedia species pageviews for a range of dates.

    - Reads a CSV containing species names and Wikidata Q-numbers.
    - Fetches daily pageview data from Wikimedia country_project_page_historical.
    - Skips dates already present in the output CSV.
    - Saves data after processing each day (periodic saving).
    - Aggregates pageviews by species and country.

    Args:
        species_file (str): Path to CSV file with columns 'Scientific Name' and 'Wikidata Q-number'.
        output_file (str): Path to CSV file where results will be saved.
        start_date_str (str): Start date in YYYY-MM-DD format.
        end_date_str (str): End date in YYYY-MM-DD format.
        user_agent (str): User-Agent string for Wikimedia requests.

    Returns:
        None. Writes data to `output_file`.
    """
    # --- 1. Load species CSV and map Q-number -> species name ---
    try:
        species_df = pd.read_csv(species_file)
        q_to_name = dict(zip(species_df["Wikidata Q-number"].astype(str), species_df["Scientific Name"]))
    except FileNotFoundError:
        print(f"Species file '{species_file}' not found.")
        return
    if not q_to_name:
        print("Species file empty or missing required columns.")
        return

    # --- 2. Generate list of all dates in the requested range ---
    start_date = datetime.strptime(start_date_str, "%Y-%m-%d")
    end_date = datetime.strptime(end_date_str, "%Y-%m-%d")
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
    print(f"{len(missing_dates)} missing dates to fetch.")

    # --- 4. Setup Wikimedia base URL and headers ---
    base_url = "https://analytics.wikimedia.org/published/datasets/country_project_page_historical"
    headers = {"User-Agent": user_agent}

    # --- 5. Process each missing date ---
    for date_str in missing_dates:
        url = f"{base_url}/{date_str}.tsv"
        print(f"Fetching data for {date_str}...")

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

            # Filter for species of interest and convert pageviews to numeric
            day_df["Wikidata Q-number"] = day_df["Wikidata Q-number"].astype(str)
            day_df["Pageviews"] = pd.to_numeric(day_df["Pageviews"], errors="coerce").fillna(0)
            day_filtered = day_df[day_df["Wikidata Q-number"].isin(q_to_name.keys())]

            # Aggregate pageviews by country and species
            day_grouped = day_filtered.groupby(["Country", "Wikidata Q-number"])["Pageviews"].sum().reset_index()

            if not day_grouped.empty:
                # Map Q-number to species name
                day_grouped["Scientific Name"] = day_grouped["Wikidata Q-number"].map(q_to_name)

                # Prepare daily data in wide format
                day_data = day_grouped[["Scientific Name", "Country", "Wikidata Q-number", "Pageviews"]]
                day_data = day_data.rename(columns={"Pageviews": date_str})

                # Merge daily data with existing CSV
                wide_df = pd.merge(
                    wide_df,
                    day_data,
                    on=["Scientific Name", "Country", "Wikidata Q-number"],
                    how="outer"
                )

                # Fill missing values with 0
                date_cols = [c for c in wide_df.columns if c not in ["Scientific Name", "Country", "Wikidata Q-number"]]
                wide_df[date_cols] = wide_df[date_cols].fillna(0)

                # Save after each date (periodic saving)
                wide_df.to_csv(output_file, index=False)
                print(f"Saved data for {date_str} to {output_file}")
            else:
                print(f"No relevant data for {date_str}, skipping.")

        except requests.exceptions.RequestException as e:
            print(f"Error fetching {date_str}: {e}. Skipping.")
        except pd.errors.EmptyDataError:
            print(f"Empty data for {date_str}, skipping.")
        except Exception as e:
            print(f"Unexpected error for {date_str}: {e}. Skipping.")

        # Polite delay to avoid hammering the server
        time.sleep(random.uniform(1, 3))

    print(f"All available data processed. Final CSV at {output_file}.")


def main():
    """Main function: load environment variables and run the fetcher."""
    load_dotenv()
    WIKI_USER_AGENT = os.getenv("WIKI_USER_AGENT")

    # Configuration
    SPECIES_FILE = "species_q_numbers.csv"
    OUTPUT_FILE = "species_pageviews_wiki_geolocated_2017-02-09_2023-02-05.csv"
    START_DATE = "2017-02-09"
    END_DATE = "2023-02-05"

    if not WIKI_USER_AGENT:
        print("Warning: WIKI_USER_AGENT not set. Add it to .env.")
        return

    fetch_and_process_pageviews(
        species_file=SPECIES_FILE,
        output_file=OUTPUT_FILE,
        start_date_str=START_DATE,
        end_date_str=END_DATE,
        user_agent=WIKI_USER_AGENT
    )


if __name__ == "__main__":
    main()
