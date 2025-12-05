import re
import requests
import pandas as pd

EU_CONCERN_URL = 'https://easin.jrc.ec.europa.eu/apixg/catxg/euconcern'


def fetch_easin_presence(
    input_csv: str,
    output_csv: str,
    eu_concern_url: str = EU_CONCERN_URL
) -> tuple[list[dict], list[str]]:
    """
    Fetch EU-concern species presence by country and write to a CSV.

    Args:
        input_csv (str): Path to CSV containing a list of species of Union concern.
                         Must include a column 'Scientific Name' or 'scientific_name'.
        output_csv (str): Path to CSV where results will be saved.
        eu_concern_url (str, optional): EASIN API URL for EU-concern species. Defaults to EU_CONCERN_URL.

    Returns:
        tuple[list[dict], list[str]]: 
            - rows: List of dicts, each representing a species-country presence entry.
            - missing_species: List of species from input that had no confirmed match in EASIN.
    """
    # 1. Load CSV
    species_df = pd.read_csv(input_csv)
    lower_columns = {column.lower(): column for column in species_df.columns}
    species_column_name = lower_columns.get('scientific name') or lower_columns.get('scientific_name')
    if not species_column_name:
        raise KeyError("Input CSV needs a 'Scientific Name' or 'scientific_name' column")
    
    species_list = sorted(species_df[species_column_name].str.strip())

    # 2. Fetch EU concern species from API
    response = requests.get(eu_concern_url)
    response.raise_for_status()
    api_records = response.json()

    # 3. Helper function to normalize species names
    def normalize_species_name(name: str) -> str:
        """Remove authorship of species and lowercase the name."""
        return re.sub(r"\s*\(.*?\)", "", (name or "")).strip().lower()

    # 4. Build presence and record maps
    presence_map: dict[str, set[str]] = {}
    record_map: dict[str, dict] = {}

    for record in api_records:
        present_countries = {
            country_info['Country']
            for country_info in (record.get('PresentInCountries') or [])
            if country_info.get('Country')
        }

        name_keys = [record.get('Name'), record.get('EUConcernName')]
        name_keys += [syn.get('Synonym') for syn in (record.get('Synonyms') or [])]

        for raw_name in name_keys:
            if isinstance(raw_name, str) and raw_name.strip():
                normalized_name = normalize_species_name(raw_name)
                presence_map[normalized_name] = present_countries
                record_map.setdefault(normalized_name, record)

    # 5. All countries observed across all species
    all_countries = sorted({country for countries in presence_map.values() for country in countries})

    # 6. Assemble rows for CSV
    rows: list[dict] = []
    missing_species: list[str] = []

    for species in species_list:
        normalized_species = normalize_species_name(species)
        species_presence = presence_map.get(normalized_species, set())
        species_record = record_map.get(normalized_species)

        # Fallback to partial matches if exact match not found
        if not species_presence:
            matching_candidates = [
                (key, record)
                for key, record in record_map.items()
                if normalized_species in key or key in normalized_species
            ]
            if matching_candidates:
                match_key, species_record = matching_candidates[0]
                species_presence = presence_map[match_key]
            else:
                missing_species.append(species)

        easin_id = species_record.get('EASINID') if species_record else None

        for country in all_countries:
            rows.append({
                'scientific_name': species,
                'easin_id': easin_id,
                'country': country,
                'present': 'yes' if country in species_presence else 'no'
            })

    # 7. Save to CSV
    pd.DataFrame(rows).to_csv(output_csv, index=False)
    return rows, missing_species


if __name__ == "__main__":
    input_csv_path = 'list_of_union_concern.csv'  # Input file of species
    output_csv_path = 'species_by_country_presence_EASIN.csv'  # Output file

    rows, missing_species = fetch_easin_presence(input_csv_path, output_csv_path)

    print(f"\nDone. {len(rows)} rows written to {output_csv_path}.")
    if missing_species:
        print("\nSpecies with no confirmed presence (and suggested matches above):")
        for species in missing_species:
            print(" -", species)
