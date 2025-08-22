#load dependencies
import requests
import csv
import time
import os
import pandas as pd
from datetime import datetime

#specify the inaturalist API endpoint
base_url = "https://api.inaturalist.org/v1/observations"

# Ensure the folder exists
output_folder = 'species_inat_observations_onlycasual'
os.makedirs(output_folder, exist_ok=True)

# Function to fetch observations per year
def fetch_new_observations(taxon_name, year, place_id, per_page=200):
    observations = []
    page = 1
    seen_ids = set()  # Track IDs to avoid duplicates

    # Load existing IDs from CSV
    species_file_name = f"{taxon_name.replace(' ', '_')}_observations.csv"
    species_file_path = os.path.join(output_folder, species_file_name)

    if os.path.exists(species_file_path):
        df_existing = pd.read_csv(species_file_path, usecols=['id'])
        seen_ids.update(df_existing['id'].astype(str))  # track all existing IDs

    consecutive_empty_pages = 0  # Track consecutive pages with no new data

    start_date = f"{year}-01-01"
    end_date = datetime.today().strftime('%Y-%m-%d') if year == datetime.today().year else f"{year}-12-31"

    while True:
        print(f"Fetching {year}, page {page} for {taxon_name} (non-research grade)...")

        response = requests.get(
            base_url,
            params={
                'taxon_name': taxon_name,
                'd1': start_date,
                'd2': end_date,
                'place_id': place_id,
                'order_by': 'observed_on',
                'order': 'asc',
                'per_page': per_page,
                'page': page,
                'quality_grade': 'casual,needs_id' # Added to exclude research grade observations
            }
        )

        if response.status_code != 200:
            print(f"Error fetching data: {response.status_code} for {taxon_name} ({year})")
            break

        # Check rate limit
        remaining_requests = int(response.headers.get('X-RateLimit-Remaining', 0))
        reset_time = int(response.headers.get('X-RateLimit-Reset', time.time()))

        if remaining_requests == 0:
            sleep_time = reset_time - time.time() + 1
            print(f"Rate limit reached for {taxon_name} ({year}). Sleeping for {sleep_time:.2f} seconds.")
            time.sleep(sleep_time)

        # Process data
        data = response.json()
        results = data.get('results', [])

        # Filter out already-seen IDs
        new_observations = [obs for obs in results if str(obs['id']) not in seen_ids]

        if not new_observations:
            consecutive_empty_pages += 1
            print(f"No new observations on page {page} for {taxon_name} ({year}). ({consecutive_empty_pages}/51)")
            if consecutive_empty_pages >= 51:  # Stop if 51 consecutive pages (max for 10k results) return no new data
                break
        else:
            consecutive_empty_pages = 0  # Reset empty page counter if new data is found

        # Add new IDs to the set
        seen_ids.update(str(obs['id']) for obs in new_observations)

        observations.extend(new_observations)

        # If fewer results than `per_page`, we're done
        if len(results) < per_page:
            break

        page += 1
        time.sleep(1)  # Avoid hitting rate limits

    print(f"Fetched {len(observations)} new observations for {taxon_name} in {year}.")
    return observations

# Function to write new observations to CSV (no changes needed here)
def write_observations_to_csv(species_name, observations):
    if not observations:
        print(f"No new observations to write for {species_name}.")
        return

    species_file_name = f"{species_name.replace(' ', '_')}_observations.csv"
    species_file_path = os.path.join(output_folder, species_file_name)

    file_exists = os.path.exists(species_file_path)

    with open(species_file_path, mode='a' if file_exists else 'w', newline='', encoding='utf-8') as file:
        writer = csv.writer(file)

        # Write header if it's a new file
        if not file_exists:
            writer.writerow(['id', 'species_guess', 'observed_on', 'place_guess', 'latitude', 'longitude', 'quality'])

        for obs in observations:
            obs_id = obs['id']
            species_guess = obs.get('species_guess', '')
            observed_on = obs.get('observed_on', '')
            place_guess = obs.get('place_guess', '')
            coords = obs.get("geojson", {}).get("coordinates", [None, None])
            longitude, latitude = coords if coords else (None, None)
            quality = obs.get('quality_grade', '')

            writer.writerow([obs_id, species_guess, observed_on, place_guess, latitude, longitude, quality])

    print(f"Appended {len(observations)} new observations for {species_name} to CSV.")

# Read the species list
species_df = pd.read_csv('species_q_numbers.csv')
species_list = species_df['Scientific Name'].unique()

# Define search parameters
place_id = 97391  # Replace with actual place ID for Europe
start_year = 2016
current_year = datetime.today().year
years = list(range(start_year, current_year + 1))  # Fetch from 2022 until today

# Iterate through each species
for species_name in species_list:
    for year in years:
        print(f"Fetching observations for {species_name} in {year}...")
        observations = fetch_new_observations(species_name, year, place_id)
        write_observations_to_csv(species_name, observations)

print("Script has finished processing all species.")