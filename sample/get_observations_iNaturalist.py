import requests
import pandas as pd
from datetime import datetime, timedelta
import time
import sys

def get_taxon_id(species_name):
    """
    Given a species name, this function queries the iNaturalist /v1/taxa endpoint
    and returns the taxon ID. If no match is found, it returns None.
    """
    base_url = "https://api.inaturalist.org/v1/taxa"
    params = {
        "q": species_name,
        "rank": "species",
        "per_page": 1  # only need the top match
    }
    response = requests.get(base_url, params=params)
    if response.status_code == 200:
        data = response.json()
        if data.get("total_results", 0) > 0:
            return data["results"][0]["id"]
    return None

def get_country_place_id(country_name):
    """
    Uses the iNaturalist places autocomplete endpoint to fetch the place_id for a given country.
    Returns the first result's id if found, else returns None.
    """
    base_url = "https://api.inaturalist.org/v1/places/autocomplete"
    params = {"q": country_name, "per_page": 1}
    response = requests.get(base_url, params=params)
    if response.status_code == 200:
        data = response.json()
        results = data.get("results", [])
        if results:
            return results[0]["id"]
    return None

def get_observation_count_for_country(taxon_id, place_id, start_date, end_date):
    """
    Given a taxon ID, a place_id (for a country), and a date range,
    this function queries the iNaturalist /v1/observations endpoint
    and returns the total number of observations.
    """
    base_url = "https://api.inaturalist.org/v1/observations"
    params = {
        "taxon_id": taxon_id,
        "place_id": place_id,
        "d1": start_date,
        "d2": end_date,
        "per_page": 1  # we only need the count from the metadata
    }
    response = requests.get(base_url, params=params)
    if response.status_code == 200:
        data = response.json()
        return data.get("total_results", 0)
    return 0

def main(csv_file):
    # Attempt to read the CSV file
    try:
        df = pd.read_csv(csv_file)
        print(f"Successfully read '{csv_file}'. Columns found: {list(df.columns)}")
    except Exception as e:
        print(f"Error reading '{csv_file}': {e}")
        sys.exit(1)

    # Check that the expected column is present
    if "Scientific Name" not in df.columns:
        print("Error: The column 'Scientific Name' was not found in the CSV file.")
        print("Please verify that the CSV file contains a column named exactly 'Scientific Name'.")
        sys.exit(1)

    # Remove duplicate species so each unique species is processed only once.
    df_unique = df.drop_duplicates(subset=["Scientific Name"]).copy()
    print(f"Found {len(df_unique)} unique species to process.")

    # EU member states list (current as of now)
    eu_countries = [
        "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
        "Czech Republic", "Denmark", "Estonia", "Finland", "France",
        "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia",
        "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland",
        "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden"
    ]

    # Pre-fetch place_ids for each EU country
    country_place_ids = {}
    print("\nFetching place IDs for EU countries...")
    for country in eu_countries:
        place_id = get_country_place_id(country)
        if place_id is not None:
            country_place_ids[country] = place_id
            print(f"  {country}: place_id {place_id}")
        else:
            print(f"  {country}: place_id not found, skipping.")
        time.sleep(0.5)

    results = []

    # Define the date range (past 30 days)
    end_date = datetime.now().date()
    start_date = end_date - timedelta(days=30)
    start_date_str = start_date.strftime("%Y-%m-%d")
    end_date_str = end_date.strftime("%Y-%m-%d")

    # Process each unique species from the CSV file
    for index, row in df_unique.iterrows():
        scientific_name = row["Scientific Name"]
        print(f"\nProcessing species: {scientific_name}")
        taxon_id = get_taxon_id(scientific_name)
        if taxon_id is None:
            print(f"  -> Taxon ID not found for {scientific_name}. Skipping.")
            continue

        # For each EU country, get the observation count
        for country, place_id in country_place_ids.items():
            count = get_observation_count_for_country(taxon_id, place_id, start_date_str, end_date_str)
            print(f"  -> {country}: {count} observations")
            results.append({
                "Scientific Name": scientific_name,
                "taxon_id": taxon_id,
                "country": country,
                "observation_count": count
            })
            time.sleep(0.5)  # brief pause to respect API limits

    # Save the results to a CSV file
    out_df = pd.DataFrame(results)
    output_csv = "species_observation_counts_by_country.csv"
    out_df.to_csv(output_csv, index=False)
    print(f"\nResults saved to '{output_csv}'")

if __name__ == "__main__":
    # If no CSV file is provided as an argument, default to 'species_wikipedia_sitelinks.csv'
    if len(sys.argv) < 2:
        csv_file = "species_wikipedia_sitelinks.csv"
        print(f"No CSV file argument provided. Defaulting to '{csv_file}'")
    else:
        csv_file = sys.argv[1]
    main(csv_file)
