import os
import time
import csv
import sys
from datetime import datetime
from typing import List, Dict, Optional, Tuple
import requests
import pandas as pd
from tqdm.auto import tqdm # Import tqdm for progress bars

# --- Configuration Constants (Made mutable for notebook flexibility) ---
# NOTE: Removed hardcoded global OUTPUT_FOLDER creation here to move it into the pipeline function.
BASE_URL: str = "https://api.inaturalist.org/v1/observations"
TAXA_URL: str = "https://api.inaturalist.org/v1/taxa"


# --- Utility Functions ---

def get_taxon_id(scientific_name: str) -> Optional[int]:
    """
    Resolve a scientific name to its iNaturalist taxon ID using the TAXA_URL endpoint.
    This ID ensures observations for all synonyms are included.
    Returns None if not found, falling back to querying by name.
    """
    try:
        # Use only 'q' (query) parameter for simple name resolution
        response = requests.get(TAXA_URL, params={"q": scientific_name})
        response.raise_for_status() # Raise an exception for bad status codes
        
        results = response.json().get("results", [])
        if results and results[0].get("id"):
            taxon_id = results[0]["id"]
            return taxon_id
        
    except requests.exceptions.RequestException as e:
        print(f"Error resolving taxon ID for {scientific_name}: {e}")
    except Exception as e:
        print(f"An unexpected error occurred during taxon resolution for {scientific_name}: {e}")
    return None


def fetch_observations_by_date_range(
    taxon_name: str,
    start_date: str,
    end_date: str,
    place_id: int,
    per_page: int = 200,
    output_folder: str = "species_inat_observations_nonresearch"
) -> List[Dict]:
    """
    Fetch iNaturalist observations for a given species and date range (non-research grade: casual + needs_id).
    Automatically resolves to taxon_id first for full synonym coverage.

    NOTE: The 'year' parameter from the original script has been replaced by explicit date strings (d1, d2).
    """
    observations: List[Dict] = []
    page: int = 1
    seen_ids: set = set()

    # Resolve to taxon_id for synonym inclusion
    taxon_id = get_taxon_id(taxon_name)
    
    # Load existing IDs to avoid duplicates across fetches
    species_file_name = f"{taxon_name.replace(' ', '_')}_observations.csv"
    species_file_path = os.path.join(output_folder, species_file_name)
    if os.path.exists(species_file_path):
        try:
            df_existing = pd.read_csv(species_file_path, usecols=['id'], low_memory=False)
            seen_ids.update(df_existing['id'].astype(str))
        except Exception as e:
            print(f"Error reading existing CSV {species_file_path}: {e}")

    consecutive_empty_pages: int = 0

    while True:
        # Construct parameters, prioritizing taxon_id
        params = {
            "d1": start_date, # Start date string
            "d2": end_date,   # End date string
            "place_id": place_id,
            "order_by": "observed_on",
            "order": "asc",
            "per_page": per_page,
            "page": page,
            "quality_grade": "casual,needs_id",
        }
        if taxon_id:
            params["taxon_id"] = taxon_id
        else:
            params["taxon_name"] = taxon_name # Fallback to taxon_name if ID resolution fails

        response = requests.get(BASE_URL, params=params)

        if response.status_code != 200:
            print(f"Error fetching data: {response.status_code} for {taxon_name} (d1:{start_date}, d2:{end_date})")
            break

        # Rate limit handling
        remaining_requests: int = int(response.headers.get("X-RateLimit-Remaining", 1))
        reset_time: int = int(response.headers.get("X-RateLimit-Reset", time.time()))
        if remaining_requests == 0:
            sleep_time: float = max(reset_time - time.time() + 1, 0)
            print(f"Rate limit reached. Sleeping for {sleep_time:.2f}s.")
            time.sleep(sleep_time)

        data: Dict = response.json()
        results: List[Dict] = data.get("results", [])
        new_obs: List[Dict] = [obs for obs in results if str(obs["id"]) not in seen_ids]

        if not new_obs:
            consecutive_empty_pages += 1
        else:
            consecutive_empty_pages = 0

        seen_ids.update(str(obs["id"]) for obs in new_obs)
        observations.extend(new_obs)

        # Break conditions
        if not results or len(results) < per_page or consecutive_empty_pages >= 5:
            break

        page += 1
        time.sleep(0.5) # Reduced sleep to improve fetching time

    print(f"Finished fetching {len(observations)} new non-research observations for {taxon_name} (d1:{start_date}, d2:{end_date}).")
    return observations


def write_observations_to_csv(
    species_name: str,
    observations: List[Dict],
    output_folder: str # output_folder must be passed explicitly now
) -> None:
    """
    Append new non-research observations (casual + needs_id) to the species CSV file.
    """
    if not observations:
        return None

    species_file_name: str = f"{species_name.replace(' ', '_')}_observations.csv"
    species_file_path: str = os.path.join(output_folder, species_file_name)
    file_exists: bool = os.path.exists(species_file_path)

    # Simplified fields for output
    FIELD_NAMES = ['id', 'species_guess', 'observed_on', 'place_guess', 'latitude', 'longitude', 'quality']

    with open(species_file_path, mode='a' if file_exists else 'w', newline='', encoding='utf-8') as file:
        writer = csv.DictWriter(file, fieldnames=FIELD_NAMES)
        
        if not file_exists:
            writer.writeheader()

        for obs in observations:
            coords: List[float] = obs.get("geojson", {}).get("coordinates", [None, None])
            longitude, latitude = coords if coords else (None, None)
            
            # Write using dictionary mapping
            writer.writerow({
                'id': obs.get("id"),
                'species_guess': obs.get("species_guess", ""),
                'observed_on': obs.get("observed_on", ""),
                'place_guess': obs.get("place_guess", ""),
                'latitude': latitude,
                'longitude': longitude,
                'quality': obs.get("quality_grade", "")
            })

# ----------------------------------------------------------------------
## 🚀 Main Pipeline Function for Notebook Utility
# ----------------------------------------------------------------------

def run_inat_pipeline(
    species_csv_path: str,
    place_id: int,
    start_date: str = '2016-01-01',  # YYYY-MM-DD
    end_date: str = datetime.today().strftime('%Y-%m-%d'), # YYYY-MM-DD
    output_folder: str = "species_inat_observations_nonresearch"
) -> Tuple[pd.DataFrame, List[str]]:
    """
    Orchestrates the fetching and storage of iNaturalist observations
    for all species in a CSV over a defined date range.

    This function is optimized for direct use in Jupyter/Colab notebooks.

    Args:
        species_csv_path: CSV file path with a 'Scientific Name' column.
        place_id: iNaturalist place ID to search within (e.g., 97391 for the EU).
        start_date: Observation start date (YYYY-MM-DD). Defaults to '2016-01-01'.
        end_date: Observation end date (YYYY-MM-DD). Defaults to today's date.
        output_folder: Folder to store output CSV files.

    Returns:
        Tuple[pd.DataFrame, List[str]]: (The input species DataFrame, List of species names processed)
    """
    
    # 1. Setup: Create the output folder
    os.makedirs(output_folder, exist_ok=True)
    
    print(f"--- Starting iNaturalist Pipeline ---")
    print(f"Fetching observations from {start_date} to {end_date} in place_id={place_id}...")
    print(f"Output folder: {output_folder}")

    # 2. Load species list
    try:
        species_df: pd.DataFrame = pd.read_csv(species_csv_path)
        species_list: List[str] = species_df['Scientific Name'].dropna().unique().tolist()
    except FileNotFoundError:
        print(f"Error: Species CSV not found at '{species_csv_path}'. Pipeline exiting.")
        return pd.DataFrame(), []
    
    print(f"Found {len(species_list)} species to process.")
    
    processed_species: List[str] = []

    # 3. Main processing loop with progress bar
    for species_name in tqdm(species_list, desc="Overall Species Progress"):
        # The fetching function now handles the single large date range
        observations = fetch_observations_by_date_range(
            taxon_name=species_name,
            start_date=start_date,
            end_date=end_date,
            place_id=place_id,
            output_folder=output_folder
        )
        
        write_observations_to_csv(species_name, observations, output_folder=output_folder)
        processed_species.append(species_name)

    print("\n✅ Script has finished processing all species.")
    return species_df, processed_species


# ----------------------------------------------------------------------
## Main Execution Block 
# ----------------------------------------------------------------------

if __name__ == "__main__":
    # Example of how a notebook user would call this function:
    
    # Define the parameters cleanly using YYYY-MM-DD strings
    species_input = 'unionconcern_invasive_species_qnumbers_2025.csv' 
    eu_place_id = 97391 # Example: European Union
    
    # Date range for a specific project timeline (e.g., last 5 full years)
    start_date = '2016-01-01'
    end_date = '2025-07-15'
    
    # Call the function and capture the output DataFrames/Lists
    species_df_out, processed_list = run_inat_pipeline(
        species_csv_path=species_input, 
        place_id=eu_place_id,
        start_date=start_date,
        end_date=end_date
    )
    
    # Print a summary of the results
    print(f"\nTotal species processed: {len(processed_list)}")
    print("Example processed species:", processed_list[:5])