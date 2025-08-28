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
            - missing: List of species from input that had no confirmed match in EASIN.
    """
    # 1. Load CSV
    union_df = pd.read_csv(input_csv)
    cols = {c.lower(): c for c in union_df.columns}
    col_name = cols.get('scientific name') or cols.get('scientific_name')
    if not col_name:
        raise KeyError("Input CSV needs a 'Scientific Name' or 'scientific_name' column")
    union_species = sorted(union_df[col_name].str.strip())

    # 2. Fetch EU concern species
    resp = requests.get(eu_concern_url)
    resp.raise_for_status()
    records = resp.json()

    # 3. Normalize helper
    def normalize(name: str) -> str:
        """Remove authorship of species and lowercase the name."""
        return re.sub(r"\s*\(.*?\)", "", (name or "")).strip().lower()

    # 4. Build presence and record maps
    presence_map: dict[str, set[str]] = {}
    record_map: dict[str, dict] = {}
    for rec in records:
        present = {e['Country'] for e in (rec.get('PresentInCountries') or []) if e.get('Country')}
        keys = [rec.get('Name'), rec.get('EUConcernName')] + [s.get('Synonym') for s in (rec.get('Synonyms') or [])]
        for raw in keys:
            if isinstance(raw, str) and raw.strip():
                k = normalize(raw)
                presence_map[k] = present
                record_map.setdefault(k, rec)

    # 5. Countries
    all_countries = sorted({c for pres in presence_map.values() for c in pres})

    # 6. Assemble rows
    rows: list[dict] = []
    missing: list[str] = []
    for sp in union_species:
        nsp = normalize(sp)
        pres = presence_map.get(nsp, set())
        rec = record_map.get(nsp)

        if not pres:
            candidates = [(k, r) for k, r in record_map.items() if nsp in k or k in nsp]
            if candidates:
                key, rec = candidates[0]
                pres = presence_map[key]
            else:
                missing.append(sp)

        easin_id = rec.get('EASINID') if rec else None

        for country in all_countries:
            rows.append({
                'scientific_name': sp,
                'easin_id': easin_id,
                'country': country,
                'present': 'yes' if country in pres else 'no'
            })

    # 7. Save to CSV
    pd.DataFrame(rows).to_csv(output_csv, index=False)
    return rows, missing


if __name__ == "__main__":
    INPUT_CSV = 'list_of_union_concern.csv' # Input file of species
    OUTPUT_CSV = 'species_by_country_presence_EASIN.csv' # Output file

    rows, missing = fetch_easin_presence(INPUT_CSV, OUTPUT_CSV)

    print(f"\nDone. {len(rows)} rows written to {OUTPUT_CSV}.")
    if missing:
        print("\nSpecies with no confirmed presence (and suggested matches above):")
        for sp in missing:
            print(" -", sp)
