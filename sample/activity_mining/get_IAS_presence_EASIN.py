import re
import requests
import pandas as pd

# Settings
INPUT_CSV      = 'list_of_union_concern.csv'
OUTPUT_CSV     = 'species_by_country_presence_EASIN.csv'
EU_CONCERN_URL = 'https://easin.jrc.ec.europa.eu/apixg/catxg/euconcern'

# 1. Load your Union-concern list
union_df = pd.read_csv(INPUT_CSV)
cols = {c.lower(): c for c in union_df.columns}
col_name = cols.get('scientific name') or cols.get('scientific_name')
if not col_name:
    raise KeyError("Input CSV needs a 'Scientific Name' or 'scientific_name' column")
union_species = sorted(union_df[col_name].str.strip())

# 2. Fetch all EU-concern species
resp = requests.get(EU_CONCERN_URL)
resp.raise_for_status()
records = resp.json()  # list of dicts

# 3. Normalization helper
def normalize(name: str) -> str:
    name = name or ""
    name = re.sub(r"\s*\(.*?\)", "", name)  # strip authorship
    return name.strip().lower()

# 4. Build primary presence & record maps
presence_map = {}
record_map = {}  # norm_key -> full record
for rec in records:
    present = {
        e['Country'] for e in (rec.get('PresentInCountries') or [])
        if e.get('Country')
    }
    # index by Name, EUConcernName, and each Synonym
    keys = [rec.get('Name'), rec.get('EUConcernName')] + \
           [s.get('Synonym') for s in (rec.get('Synonyms') or [])]
    for raw in keys:
        if isinstance(raw, str) and raw.strip():
            k = normalize(raw)
            presence_map[k] = present
            # we only need one record per key
            record_map.setdefault(k, rec)

# 5. Fixed country list 
all_countries = sorted({c for pres in presence_map.values() for c in pres})

# 6. Assemble, with EASIN ID and fallback handling
rows = []
missing = []

for sp in union_species:
    nsp = normalize(sp)
    pres = presence_map.get(nsp, set())
    rec  = record_map.get(nsp)

    # if truly no presences, try substring matches
    if not pres:
        candidates = []
        for key, r in record_map.items():
            if nsp in key or key in nsp:
                candidates.append((key, r))
        if candidates:
            # Automatically take the first match
            key, rec = candidates[0]
            pres = presence_map[key]
        else:
            missing.append(sp)

    easin_id = rec.get('EASINID') if rec else None

    for country in all_countries:
        rows.append({
            'scientific_name': sp,
            'easin_id':        easin_id,
            'country':         country,
            'present':         'yes' if country in pres else 'no'
        })

# 7. Write out CSV
pd.DataFrame(rows).to_csv(OUTPUT_CSV, index=False)

print(f"\nDone. {len(rows)} rows written to {OUTPUT_CSV}.")
if missing:
    print("\nSpecies with no confirmed presence (and suggested matches above):")
    for sp in missing:
        print(" -", sp)
