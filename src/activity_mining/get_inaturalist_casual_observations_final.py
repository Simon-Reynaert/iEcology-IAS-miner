import os
import time
import csv
from datetime import datetime
from typing import List, Dict
import requests
import pandas as pd

# Output folder for CSVs
OUTPUT_FOLDER: str = "species_inat_observations_onlycasual_test"
os.makedirs(OUTPUT_FOLDER, exist_ok=True)

# iNaturalist API endpoint
BASE_URL: str = "https://api.inaturalist.org/v1/observations"


def fetch_new_observations(
    taxon_name: str,
    year: int,
    place_id: int,
    per_page: int = 200,
    output_folder: str = OUTPUT_FOLDER
) -> List[Dict]:
    """
    Fetch iNaturalist observations for a given species and year (non-research grade only),
    respecting rate limits and avoiding duplicates from existing CSVs.

    Args:
        taxon_name: Scientific name of the species.
        year: Year of observations.
        place_id: iNaturalist place ID to search within.
        per_page: Number of observations per API request.
        output_folder: Folder containing existing CSVs.

    Returns:
        List of new observation dictionaries.
    """
    observations: List[Dict] = []
    page: int = 1
    seen_ids: set = set()

    # Load existing IDs to avoid duplicates
    species_file_name = f"{taxon_name.replace(' ', '_')}_observations.csv"
    species_file_path = os.path.join(output_folder, species_file_name)
    if os.path.exists(species_file_path):
        df_existing = pd.read_csv(species_file_path, usecols=['id'])
        seen_ids.update(df_existing['id'].astype(str))

    consecutive_empty_pages: int = 0
    start_date: str = f"{year}-01-01"
    end_date: str = datetime.today().strftime('%Y-%m-%d') if year == datetime.today().year else f"{year}-12-31"

    # Loop through pages
    while True:
        response = requests.get(
            BASE_URL,
            params={
                "taxon_name": taxon_name,
                "d1": start_date,
                "d2": end_date,
                "place_id": place_id,
                "order_by": "observed_on",
                "order": "asc",
                "per_page": per_page,
                "page": page,
                "quality_grade": "casual,needs_id"
            }
        )

        if response.status_code != 200:
            print(f"Error fetching data: {response.status_code} for {taxon_name} ({year})")
            break

        # Rate limit handling
        remaining_requests: int = int(response.headers.get("X-RateLimit-Remaining", 0))
        reset_time: int = int(response.headers.get("X-RateLimit-Reset", time.time()))
        if remaining_requests == 0:
            sleep_time: float = max(reset_time - time.time() + 1, 0)
            print(f"Rate limit reached for {taxon_name} ({year}). Sleeping for {sleep_time:.2f}s.")
            time.sleep(sleep_time)

        # Process results
        data: Dict = response.json()
        results: List[Dict] = data.get("results", [])
        new_obs: List[Dict] = [obs for obs in results if str(obs["id"]) not in seen_ids]

        if not new_obs:
            consecutive_empty_pages += 1
        else:
            consecutive_empty_pages = 0

        seen_ids.update(str(obs["id"]) for obs in new_obs)
        observations.extend(new_obs)

        # Break if no more results or too many empty pages
        if not results or len(results) < per_page or consecutive_empty_pages >= 5:
            break

        page += 1
        time.sleep(1)  # avoid hammering the API

    print(f"Finished fetching {len(observations)} new observations for {taxon_name} in {year}.")
    return observations


def write_observations_to_csv(
    species_name: str,
    observations: List[Dict],
    output_folder: str = OUTPUT_FOLDER
) -> None:
    """
    Append new observations to the species CSV file.

    Args:
        species_name: Scientific name of the species.
        observations: List of observations returned by fetch_new_observations.
        output_folder: Folder to write CSV files.
    """
    if not observations:
        print(f"No new observations to write for {species_name}.")
        return

    species_file_name: str = f"{species_name.replace(' ', '_')}_observations.csv"
    species_file_path: str = os.path.join(output_folder, species_file_name)
    file_exists: bool = os.path.exists(species_file_path)

    with open(species_file_path, mode='a' if file_exists else 'w', newline='', encoding='utf-8') as file:
        writer = csv.writer(file)
        if not file_exists:
            writer.writerow(['id', 'species_guess', 'observed_on', 'place_guess', 'latitude', 'longitude', 'quality'])

        for obs in observations:
            coords: List[float] = obs.get("geojson", {}).get("coordinates", [None, None])
            longitude, latitude = coords if coords else (None, None)
            writer.writerow([
                obs.get("id"),
                obs.get("species_guess", ""),
                obs.get("observed_on", ""),
                obs.get("place_guess", ""),
                latitude,
                longitude,
                obs.get("quality_grade", "")
            ])

    print(f"Appended {len(observations)} new casual observations for {species_name} to CSV.")


def main(
    species_csv: str,
    place_id: int,
    start_year: int = 2016,
    output_folder: str = OUTPUT_FOLDER
) -> None:
    """
    Main function to fetch and store iNaturalist observations for all species in a CSV.

    Args:
        species_csv: CSV file with a 'Scientific Name' column listing species to fetch.
        place_id: iNaturalist place ID to search within.
        start_year: Year to start fetching from (inclusive).
        output_folder: Folder to store CSV files.
    """
    species_df: pd.DataFrame = pd.read_csv(species_csv)
    species_list: List[str] = species_df['Scientific Name'].unique()
    current_year: int = datetime.today().year
    years: List[int] = list(range(start_year, current_year + 1))

    for species_name in species_list:
        for year in years:
            print(f"Fetching casual observations for {species_name} in {year}...")
            observations = fetch_new_observations(species_name, year, place_id, output_folder=output_folder)
            write_observations_to_csv(species_name, observations, output_folder=output_folder)

    print("Script has finished processing all species.")


if __name__ == "__main__":
    main('species_q_numbers.csv', place_id=97391)
