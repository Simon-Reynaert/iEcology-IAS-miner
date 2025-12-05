# load dependencies
import pandas as pd
import concurrent.futures
import time
from tqdm.auto import tqdm # Import tqdm for notebooks/environments
import pygbif.species

# Expanded EU language codes (supporting both 2-letter and 3-letter codes)
ALLOWED_LANGS = {
    "bg", "bul", "hr", "hrv", "cs", "ces", "da", "dan", "nl", "nld",
    "en", "eng", "et", "est", "fi", "fin", "fr", "fra", "de", "deu",
    "el", "ell", "hu", "hun", "ga", "gle", "it", "ita", "lv", "lav",
    "lt", "lit", "mt", "mlt", "pl", "pol", "pt", "por", "ro", "ron",
    "sk", "slk", "sl", "slv", "es", "spa", "sv", "swe"
}

# --- GBIF Data Functions (using pygbif) ---

def get_gbif_species_key(scientific_name):
    """Fetch GBIF species key using pygbif.species.name_backbone."""
    try:
        match = pygbif.species.name_backbone(name=scientific_name)
        if match and match.get("status") in ["ACCEPTED", "SYNONYM", "DOUBTFUL"] and "usageKey" in match:
            return match["usageKey"]
        return None
    except Exception as e:
        # Use simple print for critical errors only
        print(f"❌ Error matching GBIF key for {scientific_name}: {e}")
        return None

def get_gbif_synonyms(species_key):
    """Fetch GBIF synonyms using pygbif.species.name_usage."""
    if not species_key:
        return set()
    try:
        usage_data = pygbif.species.name_usage(key=species_key, data="synonyms")
        results = usage_data.get("results", [])
        synonyms = {r.get("scientificName") for r in results if r.get("scientificName")}
        return synonyms
    except Exception as e:
        print(f"❌ Error fetching GBIF synonyms for {species_key}: {e}")
        return set()

def get_gbif_common_names(species_key):
    """Fetch common names from GBIF in allowed languages using pygbif."""
    if not species_key:
        return set()
    
    common_names = set()
    
    try:
        usage_data = pygbif.species.name_usage(key=species_key, data="vernacularNames")
        results = usage_data.get("results", [])
        
        for record in results:
            name = record.get("vernacularName")
            lang = record.get("language", "").lower()

            if name and (lang in ALLOWED_LANGS or lang[:2] in ALLOWED_LANGS):
                common_names.add((name, lang))

    except Exception as e:
        print(f"❌ Error fetching GBIF common names for {species_key}: {e}")

    # The entire language warning block is removed here.
    return common_names

def process_species(scientific_name):
    """Process a single species: fetch synonyms & common names (GBIF only)."""
    species_key = get_gbif_species_key(scientific_name)
    
    if not species_key:
        return None

    gbif_synonyms = get_gbif_synonyms(species_key)
    common_names = get_gbif_common_names(species_key)

    return {
        "scientific_name": scientific_name,
        "synonyms": list(gbif_synonyms),  
        "common_names": list(common_names)  
    }

# ----------------------------------------------------------------------
# --- Main Function for Notebook/Module Use ---
# ----------------------------------------------------------------------

def fetch_gbif_names_and_synonyms(input_csv_path: str, output_csv_path: str, max_workers: int = 10):
    """
    Fetches GBIF synonyms and common names for scientific names in an input CSV.
    
    Args:
        input_csv_path: Path to the input CSV with a column named "Scientific Name".
        output_csv_path: Path where the resulting CSV will be saved.
        max_workers: Maximum number of worker threads for concurrent processing.
        
    Returns:
        pandas.DataFrame: The resulting DataFrame or None on failure.
    """
    print(f"🚀 Starting GBIF synonyms and common names fetching for species in: **{input_csv_path}**")
    
    # 1. Read input CSV
    try:
        species_df = pd.read_csv(input_csv_path, usecols=["Scientific Name"])
        species_list = species_df["Scientific Name"].dropna().unique()
    except FileNotFoundError:
        print(f"❌ Error: Input file '{input_csv_path}' not found. Please ensure it exists.")
        return None
    except KeyError:
        print(f"❌ Error: Input file must contain a column named 'Scientific Name'.")
        return None

    num_species = len(species_list)
    if num_species == 0:
        print("⚠️ No scientific names found to process.")
        return None

    print(f"🔍 Found **{num_species}** unique scientific names. Processing with {max_workers} workers...")
    
    # 2. Multithreaded Data Collection
    start_time = time.time()
    results = []

    with concurrent.futures.ThreadPoolExecutor(max_workers=max_workers) as executor:
        future_to_species = {executor.submit(process_species, sp): sp for sp in species_list}
        
        # Use tqdm to monitor progress
        for future in tqdm(concurrent.futures.as_completed(future_to_species), total=num_species, desc="Fetching GBIF Data"):
            try:
                result = future.result()
                if result:
                    results.append(result)
            except Exception as e:
                species = future_to_species[future]
                print(f"❌ Failed to process {species}: {e}")

    print(f"\n✅ Data collection completed in {time.time() - start_time:.2f} seconds.")

    # 3. Format and Save Results
    output_data = []

    for entry in results:
        sci_name = entry["scientific_name"]
        
        for synonym in entry["synonyms"]:
            output_data.append([sci_name, synonym, "Synonym", ""])
        
        for common_name, lang in entry["common_names"]:
            output_data.append([sci_name, common_name, "Common Name", lang])

    df_output = pd.DataFrame(output_data, columns=["Scientific Name", "Name", "Type", "Language (optional)"])
    df_output.to_csv(output_csv_path, index=False, encoding="utf-8")

    print(f"📂 Saved results to **{output_csv_path}** (Total rows: {len(df_output)})")
    
    return df_output

# ----------------------------------------------------------------------
# --- Standalone Execution (main function with default files) ---
# ----------------------------------------------------------------------

def main():
    """Executes the script using default file paths and worker count."""
    
    # --- Define Default Fallback Files and Settings ---
    input_file = "species_wikipedia_sitelinks.csv"
    output_file = "GBIF_unionlist_synonyms.csv"
    max_workers = 10
    
    print("--- Standalone GBIF Processor ---")
    print(f"Using default Input: '{input_file}'")
    print(f"Using default Output: '{output_file}'")
    print(f"Using Max Workers: {max_workers}")
    
    fetch_gbif_names_and_synonyms(
        input_csv_path=input_file, 
        output_csv_path=output_file, 
        max_workers=max_workers
    )

if __name__ == "__main__":
    main()