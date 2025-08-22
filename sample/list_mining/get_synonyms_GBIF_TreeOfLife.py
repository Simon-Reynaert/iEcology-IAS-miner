#import dependencies
import requests
import pandas as pd
import concurrent.futures
import time

# Expanded EU language codes (supporting both 2-letter and 3-letter codes)
ALLOWED_LANGS = {
    "bg", "bul", "hr", "hrv", "cs", "ces", "da", "dan", "nl", "nld",
    "en", "eng", "et", "est", "fi", "fin", "fr", "fra", "de", "deu",
    "el", "ell", "hu", "hun", "ga", "gle", "it", "ita", "lv", "lav",
    "lt", "lit", "mt", "mlt", "pl", "pol", "pt", "por", "ro", "ron",
    "sk", "slk", "sl", "slv", "es", "spa", "sv", "swe"
}

# Read scientific names from CSV
species_df = pd.read_csv("species_wikipedia_sitelinks.csv", usecols=["Scientific Name"])
species_list = species_df["Scientific Name"].dropna().unique()

#functions to get GBIF species keys, synonyms and common names
def get_gbif_species_key(scientific_name):
    """Fetch GBIF species key."""
    url = f"https://api.gbif.org/v1/species/match?name={scientific_name}"
    try:
        response = requests.get(url, timeout=10)
        data = response.json()
        if response.status_code == 200 and "usageKey" in data:
            return data["usageKey"]
    except Exception as e:
        print(f"❌ Error fetching GBIF key for {scientific_name}: {e}")
    return None

def get_gbif_synonyms(species_key):
    """Fetch GBIF synonyms."""
    if not species_key:
        return set()
    url = f"https://api.gbif.org/v1/species/{species_key}/synonyms"
    try:
        response = requests.get(url, timeout=10)
        if response.status_code == 200:
            return {syn["scientificName"] for syn in response.json().get("results", [])}
    except Exception as e:
        print(f"❌ Error fetching GBIF synonyms for {species_key}: {e}")
    return set()

def get_col_synonyms(scientific_name):
    """Fetch synonyms from Catalog of Life."""
    url = f"https://api.catalogueoflife.org/name/search?exact=true&q={scientific_name}"
    try:
        response = requests.get(url, timeout=10)
        if response.status_code == 200:
            data = response.json()
            for result in data.get("results", []):
                if "synonyms" in result:
                    return {syn["name"] for syn in result["synonyms"]}
    except Exception as e:
        print(f"❌ Error fetching CoL synonyms for {scientific_name}: {e}")
    return set()

def get_gbif_common_names(species_key):
    """Fetch common names from GBIF in multiple languages."""
    if not species_key:
        return set()
    
    common_names = set()
    offset, limit = 0, 250 
    found_languages = set()

    while True:
        url = f"https://api.gbif.org/v1/species/{species_key}/vernacularNames?offset={offset}&limit={limit}"
        try:
            response = requests.get(url, timeout=10)
            if response.status_code == 200:
                data = response.json()
                results = data.get("results", [])
                if not results:
                    break 

                for record in results:
                    name = record.get("vernacularName")
                    lang = record.get("language", "").lower()
                    found_languages.add(lang)

                    if name and (lang in ALLOWED_LANGS or lang[:2] in ALLOWED_LANGS):
                        common_names.add((name, lang))

                offset += limit  
            else:
                break
        except Exception as e:
            print(f"❌ Error fetching GBIF common names for {species_key}: {e}")
            break

    missing_langs = {code for code in ["lv", "lav", "pl", "pol", "hu", "hun"] if code not in found_languages}
    if missing_langs:
        print(f"⚠️ Warning: Missing common names in {missing_langs} for species key {species_key}")

    return common_names

def process_species(scientific_name):
    """Process a single species: fetch synonyms & common names."""
    species_key = get_gbif_species_key(scientific_name)
    
    if not species_key:
        print(f"⚠️ No GBIF key found for '{scientific_name}', skipping.")
        return None

    gbif_synonyms = get_gbif_synonyms(species_key)
    col_synonyms = get_col_synonyms(scientific_name)
    common_names = get_gbif_common_names(species_key)

    return {
        "scientific_name": scientific_name,
        "synonyms": list(gbif_synonyms | col_synonyms),  
        "common_names": list(common_names)  
    }

# Multithreading for speed
start_time = time.time()
results = []

with concurrent.futures.ThreadPoolExecutor(max_workers=10) as executor:
    future_to_species = {executor.submit(process_species, sp): sp for sp in species_list}
    for future in concurrent.futures.as_completed(future_to_species):
        result = future.result()
        if result:
            results.append(result)

print(f"✅ Data collection completed in {time.time() - start_time:.2f} seconds.")

# Convert results to DataFrame and save
output_data = []

for entry in results:
    sci_name = entry["scientific_name"]
    
    for synonym in entry["synonyms"]:
        output_data.append([sci_name, synonym, "Synonym", ""])
    
    for common_name, lang in entry["common_names"]:
        output_data.append([sci_name, common_name, "Common Name", lang])

df_output = pd.DataFrame(output_data, columns=["Scientific Name", "Name", "Type", "Language"])
df_output.to_csv("UnionList_extended_synonyms.csv", index=False, encoding="utf-8")

print(f"📂 Saved results to UnionList_extended_synonyms.csv")
