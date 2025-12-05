"""
PlantNet API Identification Module

This module can be used both as a standalone script and imported for use in Jupyter notebooks.
It processes plant images through the PlantNet API and filters for high-fidelity observations
where the expected species ranks first in the API response.
"""

import requests
import json
import os
import csv
import pandas as pd
from typing import Dict, List, Tuple, Optional
from dotenv import load_dotenv


class PlantNetConfig:
    """Configuration container for PlantNet API processing."""
    
    def __init__(self, 
                 base_dir: str = "PlantNet_Batch_Images", 
                 api_key: Optional[str] = None,
                 metadata_file: Optional[str] = None,
                 output_summary_file: Optional[str] = None,
                 output_high_fidelity_file: Optional[str] = None):
        """
        Initialize PlantNet configuration.
        
        Args:
            base_dir: Directory containing observation folders
            api_key: PlantNet API key (uses .env if not provided)
            metadata_file: Path to metadata CSV (defaults to base_dir/master_observations_metadata.csv)
            output_summary_file: Path for summary results CSV
            output_high_fidelity_file: Path for high-fidelity results CSV
        """
        self.base_dir = base_dir
        self.api_key = api_key or os.getenv("PLANTNET_API_KEY")
        
        if not self.api_key:
            raise ValueError("PLANTNET_API_KEY not found in .env file or environment variables.")
        
        self.project = "all"
        self.api_endpoint = f"https://my-api.plantnet.org/v2/identify/{self.project}?api-key={self.api_key}"
        
        # File paths - allow custom paths or use defaults
        self.master_metadata_file = metadata_file or os.path.join(base_dir, "master_observations_metadata.csv")
        self.identification_summary_file = output_summary_file or os.path.join(base_dir, "identification_summary_results_per_id.csv")
        self.high_fidelity_file = output_high_fidelity_file or os.path.join(base_dir, "high_fidelity_observations.csv")


def load_expected_species(config: PlantNetConfig) -> Dict[str, str]:
    """
    Load metadata and create mapping of photo_id to expected scientific_name.
    
    Args:
        config: PlantNetConfig object containing file paths
        
    Returns:
        Dictionary mapping photo_id to scientific_name
        
    Raises:
        FileNotFoundError: If metadata file doesn't exist
        KeyError: If required columns are missing
    """
    print("📋 Loading metadata to get expected species...")
    
    try:
        metadata_df = pd.read_csv(config.master_metadata_file)
        expected_species_map = dict(zip(
            metadata_df['photo_id'].astype(str), 
            metadata_df['scientific_name']
        ))
        print(f"✅ Loaded metadata for {len(expected_species_map)} observations")
        return expected_species_map
        
    except FileNotFoundError:
        print(f"❌ Error: Metadata file not found at {config.master_metadata_file}")
        raise
    except KeyError as e:
        print(f"❌ Error: Required column missing in metadata file: {e}")
        raise


def find_observation_folders(config: PlantNetConfig) -> List[str]:
    """
    Find all observation ID folders in the base directory.
    
    Args:
        config: PlantNetConfig object containing base directory
        
    Returns:
        Sorted list of observation folder names
        
    Raises:
        FileNotFoundError: If base directory doesn't exist
        ValueError: If no observation folders found
    """
    try:
        observation_ids = sorted([
            d for d in os.listdir(config.base_dir) 
            if os.path.isdir(os.path.join(config.base_dir, d))
        ])
        
        if not observation_ids:
            raise ValueError(f"No observation ID folders found in '{config.base_dir}'")
        
        print(f"✅ Found {len(observation_ids)} observation folders")
        return observation_ids
        
    except FileNotFoundError:
        print(f"❌ Error: Base directory '{config.base_dir}' not found")
        raise


def call_plantnet_api(image_path: str, api_endpoint: str) -> Tuple[bool, Optional[dict], str]:
    """
    Call PlantNet API with an image file.
    
    Args:
        image_path: Full path to the image file
        api_endpoint: PlantNet API endpoint URL
        
    Returns:
        Tuple of (success: bool, json_result: dict or None, error_message: str)
    """
    if not os.path.exists(image_path):
        return False, None, "Image file not found locally"
    
    try:
        with open(image_path, 'rb') as image_data:
            files = [('images', (os.path.basename(image_path), image_data))]
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
            error_msg = "Could not decode JSON response"
            raw_text = response.text.strip()[:500]
            return False, None, f"{error_msg} | Raw Response: {raw_text}"
        
        # Check response status
        if response.status_code == 200:
            if 'results' in json_result and json_result['results']:
                return True, json_result, ""
            else:
                return False, json_result, "API returned success but no identification results"
        else:
            error_msg = f"API Error Code {response.status_code}"
            if 'error' in json_result:
                error_msg += f": {json_result.get('error')}"
            elif 'message' in json_result:
                error_msg += f": {json_result.get('message')}"
            return False, json_result, error_msg
            
    except requests.exceptions.RequestException as e:
        return False, None, f"Network or Request Error: {e}"
    except Exception as e:
        return False, None, f"General Error: {e}"


def process_api_results(json_result: dict, expected_species: str) -> Dict:
    """
    Process PlantNet API results and check for species match.
    
    Args:
        json_result: JSON response from PlantNet API
        expected_species: Expected scientific name
        
    Returns:
        Dictionary containing top 5 results and species match information
    """
    results = {
        'top_5': [],
        'species_found': 'No',
        'species_rank': 'Not in Top 5',
        'species_score': 'N/A'
    }
    
    all_results = json_result.get('results', [])
    top_5_results = all_results[:5]
    
    for idx, result in enumerate(top_5_results, 1):
        score = result.get('score', 0.0)
        species_info = result.get('species', {})
        scientific_name = species_info.get('scientificNameWithoutAuthor', 'Unknown Species')
        common_names = species_info.get('commonNames', [])
        
        results['top_5'].append({
            'scientific_name': scientific_name,
            'score': score,
            'common_names': common_names
        })
        
        # Check for species match
        if scientific_name.lower() == expected_species.lower():
            results['species_found'] = 'Yes'
            results['species_rank'] = str(idx)
            results['species_score'] = f"{score:.6f}"
    
    return results


def create_csv_row(obs_id: str, expected_species: str, image_path: str, 
                   api_status: str, processed_results: Optional[Dict], 
                   error_message: str = "") -> List:
    """
    Create a CSV row from processing results.
    
    Args:
        obs_id: Observation ID
        expected_species: Expected scientific name
        image_path: Path to image file
        api_status: API call status
        processed_results: Results from process_api_results()
        error_message: Error message if any
        
    Returns:
        List representing a CSV row
    """
    row = [obs_id, expected_species, image_path, api_status]
    
    # Add top 5 results
    if processed_results and processed_results['top_5']:
        for idx in range(5):
            if idx < len(processed_results['top_5']):
                result = processed_results['top_5'][idx]
                common_names_str = ' | '.join(result['common_names']) if result['common_names'] else ''
                row.extend([
                    result['scientific_name'],
                    f"{result['score']:.6f}",
                    common_names_str
                ])
            else:
                row.extend(["N/A", "0.0", ""])
    else:
        for _ in range(5):
            row.extend(["N/A", "0.0", ""])
    
    # Add species matching info
    if processed_results:
        row.extend([
            processed_results['species_found'],
            processed_results['species_rank'],
            processed_results['species_score'],
            error_message
        ])
    else:
        row.extend(["No", "Not in Top 5", "N/A", error_message])
    
    return row


def process_all_observations(config: PlantNetConfig, 
                            expected_species_map: Dict[str, str],
                            observation_ids: List[str]) -> str:
    """
    Process all observations and save results to CSV.
    
    Args:
        config: PlantNetConfig object
        expected_species_map: Dictionary mapping photo_id to scientific_name
        observation_ids: List of observation folder names
        
    Returns:
        Path to the output CSV file
    """
    # CSV header
    header = ["photo_id", "Expected_Species", "Image_Used_Path", "API_Status"]
    for i in range(1, 6):
        header.extend([
            f"Top_{i}_Scientific_Name",
            f"Top_{i}_Score",
            f"Top_{i}_Common_Names"
        ])
    header.extend([
        "Expected_Species_Found",
        "Expected_Species_Rank",
        "Expected_Species_Score",
        "Error_Message"
    ])
    
    # Process observations
    with open(config.identification_summary_file, mode='w', newline='', encoding='utf-8') as file:
        writer = csv.writer(file)
        writer.writerow(header)
        
        for i, obs_id in enumerate(observation_ids):
            print(f"\n--- Processing ID {i+1}/{len(observation_ids)}: {obs_id} ---")
            
            expected_species = expected_species_map.get(obs_id, "Unknown")
            print(f"  🎯 Expected species: {expected_species}")
            
            image_filename = f"{obs_id}.jpg"
            image_path = os.path.join(config.base_dir, obs_id, image_filename)
            
            # Call API
            success, json_result, error_message = call_plantnet_api(image_path, config.api_endpoint)
            
            if success:
                api_status = "Success"
                processed_results = process_api_results(json_result, expected_species)
                
                print(f"  📊 Got {len(json_result['results'])} total results")
                for idx, result in enumerate(processed_results['top_5'], 1):
                    print(f"    {idx}. {result['scientific_name']} (Score: {result['score']:.4f})")
                
                if processed_results['species_found'] == 'Yes':
                    print(f"    ✅ MATCH! Expected species found at rank {processed_results['species_rank']}")
                else:
                    print(f"    ❌ Expected species '{expected_species}' NOT found in top 5")
            else:
                api_status = "Failure" if json_result else "Local Failure"
                processed_results = None
                print(f"  ❌ {error_message}")
            
            # Write row
            row = create_csv_row(obs_id, expected_species, image_path, 
                               api_status, processed_results, error_message)
            writer.writerow(row)
    
    print(f"\n✅ Identification summary saved to: {config.identification_summary_file}")
    return config.identification_summary_file


def filter_high_fidelity_observations(config: PlantNetConfig, 
                                      metadata_df: pd.DataFrame) -> str:
    """
    Filter observations where expected species ranked first (rank 1).
    Creates a high-fidelity dataset by merging with original metadata.
    
    Args:
        config: PlantNetConfig object
        metadata_df: Original metadata DataFrame
        
    Returns:
        Path to the high-fidelity CSV file
    """
    print("\n🔍 Filtering for high-fidelity observations (Rank 1 matches only)...")
    
    # Load identification results
    results_df = pd.read_csv(config.identification_summary_file)

    #ensure rank is a string
    results_df['Expected_Species_Rank'] = results_df['Expected_Species_Rank'].astype(str)
    
    # Filter for rank 1 matches
    high_fidelity = results_df[
        (results_df['Expected_Species_Found'] == 'Yes') & 
        (results_df['Expected_Species_Rank'] == '1')
    ].copy()
    
    print(f"  ✅ Found {len(high_fidelity)} observations where expected species ranked #1")
    
    # Merge with original metadata to get all columns
    # Ensure photo_id types match
    metadata_df['photo_id'] = metadata_df['photo_id'].astype(str)
    high_fidelity['photo_id'] = high_fidelity['photo_id'].astype(str)
    
    # Merge on photo_id
    final_df = pd.merge(
        metadata_df,
        high_fidelity[['photo_id', 'Top_1_Score', 'API_Status']],
        on='photo_id',
        how='inner'
    )
    
    # Rename score column for clarity
    final_df.rename(columns={'Top_1_Score': 'PlantNet_Confidence_Score'}, inplace=True)
    
    # Save to CSV
    final_df.to_csv(config.high_fidelity_file, index=False)
    
    print(f"  💾 High-fidelity dataset saved to: {config.high_fidelity_file}")
    print(f"  📊 Final dataset contains {len(final_df)} observations with {len(final_df.columns)} columns")
    
    return config.high_fidelity_file


def print_summary_statistics(config: PlantNetConfig):
    """
    Print summary statistics of the processing results.
    
    Args:
        config: PlantNetConfig object
    """
    print("\n📈 SUMMARY STATISTICS:")
    
    try:
        results_df = pd.read_csv(config.identification_summary_file)
        total = len(results_df)
        successful = len(results_df[results_df['API_Status'] == 'Success'])
        species_found_count = len(results_df[results_df['Expected_Species_Found'] == 'Yes'])
        rank_1_count = len(results_df[
            (results_df['Expected_Species_Found'] == 'Yes') & 
            (results_df['Expected_Species_Rank'] == '1')
        ])
        
        print(f"  Total observations processed: {total}")
        print(f"  Successful API calls: {successful} ({successful/total*100:.1f}%)")
        print(f"  Expected species found in top 5: {species_found_count} ({species_found_count/total*100:.1f}%)")
        print(f"  Expected species ranked #1 (high-fidelity): {rank_1_count} ({rank_1_count/total*100:.1f}%)")
        
        # Rank distribution
        if species_found_count > 0:
            print(f"\n  📊 Rank distribution when species found:")
            rank_counts = results_df[
                results_df['Expected_Species_Found'] == 'Yes'
            ]['Expected_Species_Rank'].value_counts().sort_index()
            
            for rank, count in rank_counts.items():
                percentage = count/species_found_count*100
                print(f"    Rank {rank}: {count} observations ({percentage:.1f}%)")
        
    except Exception as e:
        print(f"  ⚠️ Could not generate summary statistics: {e}")

# ==============================================================================
# Main workflow functions for notebook use
# ==============================================================================

def run_full_pipeline(base_dir: str = "PlantNet_Batch_Images", 
                     api_key: Optional[str] = None,
                     metadata_file: Optional[str] = None,
                     output_summary_file: Optional[str] = None,
                     output_high_fidelity_file: Optional[str] = None) -> Tuple[str, str]:
    """
    Run the complete PlantNet identification and filtering pipeline.
    
    This is the main function to call from a Jupyter notebook.
    
    Args:
        base_dir: Base directory containing observation folders
        api_key: PlantNet API key (optional, will use .env if not provided)
        metadata_file: Path to metadata CSV file (optional, defaults to base_dir/master_observations_metadata.csv)
        output_summary_file: Custom path for summary results CSV
        output_high_fidelity_file: Custom path for high-fidelity results CSV
        
    Returns:
        Tuple of (summary_file_path, high_fidelity_file_path)
        
    Examples:
        >>> # Simple usage with defaults
        >>> summary, high_fidelity = run_full_pipeline()
        
        >>> # Custom base directory
        >>> summary, high_fidelity = run_full_pipeline(base_dir="/path/to/my/data")
        
        >>> # Custom metadata file location
        >>> summary, high_fidelity = run_full_pipeline(
        ...     base_dir="my_plant_images",
        ...     metadata_file="/path/to/custom_metadata.csv"
        ... )
        
        >>> # Fully custom paths
        >>> summary, high_fidelity = run_full_pipeline(
        ...     base_dir="plant_data",
        ...     metadata_file="metadata/observations.csv",
        ...     output_summary_file="results/full_summary.csv",
        ...     output_high_fidelity_file="results/validated_observations.csv"
        ... )
    """
    # Initialize configuration
    config = PlantNetConfig(
        base_dir=base_dir, 
        api_key=api_key,
        metadata_file=metadata_file,
        output_summary_file=output_summary_file,
        output_high_fidelity_file=output_high_fidelity_file
    )
    
    # Load metadata
    expected_species_map = load_expected_species(config)
    metadata_df = pd.read_csv(config.master_metadata_file)
    
    # Find observation folders
    observation_ids = find_observation_folders(config)
    
    # Process all observations
    summary_file = process_all_observations(config, expected_species_map, observation_ids)
    
    # Filter for high-fidelity observations
    high_fidelity_file = filter_high_fidelity_observations(config, metadata_df)
    
    # Print statistics
    print_summary_statistics(config)
    
    return summary_file, high_fidelity_file


def load_high_fidelity_data(base_dir: str = "PlantNet_Batch_Images") -> pd.DataFrame:
    """
    Load the high-fidelity observations dataset.
    
    Args:
        base_dir: Base directory containing the output files
        
    Returns:
        DataFrame containing high-fidelity observations
        
    Example:
        >>> df = load_high_fidelity_data()
        >>> print(df.head())
    """
    config = PlantNetConfig(base_dir=base_dir, api_key="dummy")  # API key not needed for loading
    return pd.read_csv(config.high_fidelity_file)


# ==============================================================================
# Standalone script execution
# ==============================================================================

def main():
    """Main function for standalone script execution."""
    load_dotenv()
    
    print("🌿 PlantNet Batch Identification - High Fidelity Filtering")
    print("=" * 70)
    
    try:
        summary_file, high_fidelity_file = run_full_pipeline()
        
        print("\n" + "=" * 70)
        print("✅ PROCESSING COMPLETE!")
        print(f"\n📄 Output files:")
        print(f"  1. Full results: {summary_file}")
        print(f"  2. High-fidelity dataset: {high_fidelity_file}")
        
    except Exception as e:
        print(f"\n❌ Pipeline failed: {e}")
        raise


if __name__ == "__main__":
    main()