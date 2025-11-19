import requests
import json
import os
import csv
import pandas as pd
from dotenv import load_dotenv

# --- Configuration ---
load_dotenv()

API_KEY = os.getenv("PLANTNET_API_KEY")
if not API_KEY:
    raise ValueError("PLANTNET_API_KEY not found in .env file or environment variables.")

PROJECT = "all"
api_endpoint = f"https://my-api.plantnet.org/v2/identify/{PROJECT}?api-key={API_KEY}"

# --- File and Directory Setup ---
BASE_DIR = "PlantNet_Batch_Images"
MASTER_METADATA_FILE = os.path.join(BASE_DIR, "master_observations_metadata.csv")
IDENTIFICATION_SUMMARY_FILE = os.path.join(BASE_DIR, "identification_summary_results_per_id.csv")
FINAL_OUTPUT_FILE = os.path.join(BASE_DIR, "final_complete_analysis_per_id.csv")

# --- Load Metadata to get expected species for each photo_id ---
print("📋 Loading metadata to get expected species...")
try:
    metadata_df = pd.read_csv(MASTER_METADATA_FILE)
    # Create a dictionary mapping photo_id to scientific_name
    expected_species_map = dict(zip(metadata_df['photo_id'].astype(str), metadata_df['scientific_name']))
    print(f"✅ Loaded metadata for {len(expected_species_map)} observations")
except FileNotFoundError:
    print(f"❌ Error: Metadata file not found at {MASTER_METADATA_FILE}")
    exit()
except KeyError as e:
    print(f"❌ Error: Required column missing in metadata file: {e}")
    exit()

# --- 1. Find All Observations ---
try:
    observation_ids = sorted([
        d for d in os.listdir(BASE_DIR) 
        if os.path.isdir(os.path.join(BASE_DIR, d))
    ])
    
    if not observation_ids:
        print(f"\n❌ **Error**: No observation ID folders found in '{BASE_DIR}'.")
        exit()

    print(f"✅ Found {len(observation_ids)} observation folders. Starting individual processing...")

except FileNotFoundError:
    print(f"\n❌ **Error**: The base directory '{BASE_DIR}' was not found. Please create it.")
    exit()


# --- 2. Initialize CSV Output File with Top 5 columns ---
with open(IDENTIFICATION_SUMMARY_FILE, mode='w', newline='', encoding='utf-8') as file:
    writer = csv.writer(file)
    
    # Write header with top 5 results and species match info
    header = ["photo_id", "Expected_Species", "Image_Used_Path", "API_Status"]
    
    # Add columns for top 5 results
    for i in range(1, 6):
        header.extend([
            f"Top_{i}_Scientific_Name",
            f"Top_{i}_Score",
            f"Top_{i}_Common_Names"
        ])
    
    # Add species matching columns
    header.extend([
        "Expected_Species_Found",
        "Expected_Species_Rank",
        "Expected_Species_Score",
        "Error_Message"
    ])
    
    writer.writerow(header)
    
    # --- 3. Loop and Process Each ID ---
    for i, obs_id in enumerate(observation_ids):
        print(f"\n--- Processing ID {i+1}/{len(observation_ids)}: {obs_id} ---")
        
        # Get expected species from metadata
        expected_species = expected_species_map.get(obs_id, "Unknown")
        print(f"  🎯 Expected species: {expected_species}")
        
        image_filename = f"{obs_id}.jpg"
        image_full_path = os.path.join(BASE_DIR, obs_id, image_filename)
        
        # Initialize variables for CSV output
        api_status = "Failure"
        top_5_results = []
        error_message = ""
        
        # Variables for species matching
        species_found = "No"
        species_rank = "Not in Top 5"
        species_score = "N/A"

        if not os.path.exists(image_full_path):
            error_message = "Image file not found locally"
            print(f"⚠️ Warning: {error_message}")
            
            # Write row with empty top 5 results
            row = [obs_id, expected_species, image_full_path, "Local Failure"]
            for _ in range(5):
                row.extend(["N/A", "0.0", ""])
            row.extend(["No", "Not in Top 5", "N/A", error_message])
            writer.writerow(row)
            continue
            
        try:
            with open(image_full_path, 'rb') as image_data:
                files = [
                    ('images', (os.path.basename(image_full_path), image_data))
                ]
                data = {'organs': 'auto'}
                
                response = requests.post(
                    url=api_endpoint,
                    files=files,
                    data=data
                )

            # Attempt to decode JSON
            try:
                json_result = response.json()
            except requests.exceptions.JSONDecodeError:
                error_message = "Could not decode JSON response (API server likely sent plain text error)."
                raw_response_text = response.text.strip()
                print(f"  ❌ {error_message}")
                print(f"  🚨 RAW ERROR RESPONSE TEXT: {raw_response_text[:500]}")
                
                row = [obs_id, expected_species, image_full_path, "API Error (Raw)"]
                for _ in range(5):
                    row.extend(["N/A", "0.0", ""])
                row.extend(["No", "Not in Top 5", "N/A", error_message + f" | Raw Response: {raw_response_text[:100]}"])
                writer.writerow(row)
                continue
            
            # --- Response Logic (JSON was decoded successfully) ---
            if response.status_code == 200:
                api_status = "Success"
                if 'results' in json_result and json_result['results']:
                    # Get top 5 results (or fewer if less than 5 available)
                    all_results = json_result['results']
                    top_5_results = all_results[:5]
                    
                    print(f"  📊 Got {len(all_results)} total results, analyzing top 5...")
                    
                    # Process each of the top 5 results
                    for idx, result in enumerate(top_5_results, 1):
                        score = result.get('score', 0.0)
                        species_info = result.get('species', {})
                        scientific_name = species_info.get('scientificNameWithoutAuthor', 'Unknown Species')
                        common_names = species_info.get('commonNames', [])
                        
                        print(f"    {idx}. {scientific_name} (Score: {score:.4f})")
                        
                        # Check if this matches the expected species
                        if scientific_name.lower() == expected_species.lower():
                            species_found = "Yes"
                            species_rank = str(idx)
                            species_score = f"{score:.6f}"
                            print(f"    ✅ MATCH! Expected species found at rank {idx}")
                    
                    if species_found == "No":
                        print(f"    ❌ Expected species '{expected_species}' NOT found in top 5")
                else:
                    error_message = "API returned a success status but no identification results."
                    print(f"  ⚠️ {error_message}")
            
            else:
                api_status = "Failure"
                error_message = f"API Error Code {response.status_code}"
                
                if 'error' in json_result:
                    error_detail = json_result.get('error')
                    error_message += f": {error_detail}"
                    print(f"  ❌ **API Error Detail:** {error_detail}")
                elif 'message' in json_result:
                    error_detail = json_result.get('message')
                    error_message += f": {error_detail}"
                    print(f"  ❌ **API Error Detail (Message):** {error_detail}")
                else:
                    print(f"  ❌ **API Error (Non-Standard JSON):** Printing full result structure for inspection.")
                    print(json.dumps(json_result, indent=2))
                
                print(f"  ⚠️ API Error: {error_message}")

        except requests.exceptions.RequestException as e:
            error_message = f"Network or Request Error: {e}"
            print(f"  ❌ {error_message}")
        except Exception as e:
            error_message = f"General Error: {e}"
            print(f"  ❌ {error_message}")

        # Build the CSV row
        row = [obs_id, expected_species, image_full_path, api_status]
        
        # Add top 5 results (pad with N/A if fewer than 5)
        for idx in range(5):
            if idx < len(top_5_results):
                result = top_5_results[idx]
                score = result.get('score', 0.0)
                species_info = result.get('species', {})
                scientific_name = species_info.get('scientificNameWithoutAuthor', 'N/A')
                common_names = species_info.get('commonNames', [])
                common_names_str = ' | '.join(common_names) if common_names else ''
                
                row.extend([scientific_name, f"{score:.6f}", common_names_str])
            else:
                row.extend(["N/A", "0.0", ""])
        
        # Add species matching info
        row.extend([species_found, species_rank, species_score, error_message])
        
        writer.writerow(row)

print(f"\n\n✅ Finished all API calls. Identification summary saved to: **{IDENTIFICATION_SUMMARY_FILE}**")

# --- Summary Statistics ---
print("\n📈 SUMMARY STATISTICS:")
try:
    results_df = pd.read_csv(IDENTIFICATION_SUMMARY_FILE)
    total = len(results_df)
    successful = len(results_df[results_df['API_Status'] == 'Success'])
    species_found_count = len(results_df[results_df['Expected_Species_Found'] == 'Yes'])
    
    print(f"  Total observations processed: {total}")
    print(f"  Successful API calls: {successful}")
    print(f"  Expected species found in top 5: {species_found_count} ({species_found_count/total*100:.1f}%)")
    
    # Rank distribution
    if species_found_count > 0:
        print(f"\n  Rank distribution when found:")
        rank_counts = results_df[results_df['Expected_Species_Found'] == 'Yes']['Expected_Species_Rank'].value_counts().sort_index()
        for rank, count in rank_counts.items():
            print(f"    Rank {rank}: {count} observations")
    
except Exception as e:
    print(f"  ⚠️ Could not generate summary statistics: {e}")