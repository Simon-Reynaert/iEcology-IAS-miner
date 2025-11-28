import pandas as pd
import os
import requests
from urllib.parse import urlparse
import time
import random
from typing import List, Optional, Dict


class FlickrImageDownloader:
    """
    A class to download Flickr images for plant species observations.
    """
    
    def __init__(
        self, 
        csv_path: str,
        base_output_dir: str = "PlantNet_Batch_Images",
        master_metadata_file: Optional[str] = None
    ):
        """
        Initialize the downloader.
        
        Args:
            csv_path: Path to the CSV file containing observations
            base_output_dir: Directory where images will be saved
            master_metadata_file: Path to master metadata CSV (optional)
        """
        self.df = pd.read_csv(csv_path)
        self.base_output_dir = base_output_dir
        self.master_metadata_file = master_metadata_file or os.path.join(
            base_output_dir, "master_observations_metadata.csv"
        )
        self.metadata_list = []
        
        # Create base directory if it doesn't exist
        if not os.path.exists(self.base_output_dir):
            os.makedirs(self.base_output_dir)
            print(f"Created base directory: {self.base_output_dir}")
    
    def get_processed_species(self, processed_csv_path: Optional[str] = None) -> set:
        """
        Get list of already processed species from a CSV file.
        
        Args:
            processed_csv_path: Path to CSV containing processed species
            
        Returns:
            Set of processed species names
        """
        if processed_csv_path and os.path.exists(processed_csv_path):
            try:
                processed_df = pd.read_csv(processed_csv_path)
                if 'scientific_name' in processed_df.columns:
                    return set(processed_df['scientific_name'].unique())
            except Exception as e:
                print(f"Warning: Could not read processed species from {processed_csv_path}: {e}")
        return set()
    
    def filter_species_data(self, species_name: str) -> pd.DataFrame:
        """
        Filter data for a specific species.
        
        Args:
            species_name: Scientific name of the species
            
        Returns:
            Filtered DataFrame
        """
        species_df = self.df[self.df['scientific_name'] == species_name].copy()
        species_df.dropna(subset=['url'], inplace=True)
        return species_df
    
    def download_image(
        self, 
        url: str, 
        photo_id: str, 
        row: pd.Series,
        min_sleep: float = 0.5,
        max_sleep: float = 2.0
    ) -> bool:
        """
        Download a single image and save metadata.
        
        Args:
            url: Image URL
            photo_id: Unique photo identifier
            row: DataFrame row containing observation data
            min_sleep: Minimum sleep time between downloads
            max_sleep: Maximum sleep time between downloads
            
        Returns:
            True if successful, False otherwise
        """
        observation_folder = os.path.join(self.base_output_dir, photo_id)
        
        if not os.path.exists(observation_folder):
            os.makedirs(observation_folder)
        
        # Determine file extension
        path = urlparse(url).path
        ext = os.path.splitext(path)[1]
        if not ext or len(ext) < 2:
            ext = '.jpg'
        
        file_path = os.path.join(observation_folder, f"{photo_id}{ext}")
        
        # Skip if already exists
        if os.path.exists(file_path):
            print(f"  ⚠️ Skipping ID: {photo_id}. File already exists.")
            return False
        
        try:
            # Download image
            response = requests.get(url, timeout=10)
            response.raise_for_status()
            
            with open(file_path, 'wb') as f:
                f.write(response.content)
            
            # Add metadata
            self.metadata_list.append({
                'photo_id': photo_id,
                'scientific_name': row['scientific_name'],
                'latitude': row['latitude'],
                'longitude': row['longitude'],
                'local_folder': photo_id,
                'local_path': file_path
            })
            print(f"  ✅ Saved image and metadata for ID: {photo_id}")
            
            # Random sleep to avoid overwhelming server
            sleep_time = random.uniform(min_sleep, max_sleep)
            time.sleep(sleep_time)
            print(f"   💤 Pausing for {sleep_time:.2f} seconds...")
            
            return True
            
        except requests.exceptions.RequestException as e:
            print(f"  ❌ Failed to download {url}. Error: {e}")
            return False
        except Exception as e:
            print(f"  ❌ An unexpected error occurred for ID {photo_id}. Error: {e}")
            return False
    
    def process_species(
        self, 
        species_name: str,
        min_sleep: float = 0.5,
        max_sleep: float = 2.0
    ) -> int:
        """
        Process all observations for a single species.
        
        Args:
            species_name: Scientific name of the species
            min_sleep: Minimum sleep time between downloads
            max_sleep: Maximum sleep time between downloads
            
        Returns:
            Number of successfully downloaded images
        """
        print("\n" + "="*50)
        print(f"**Starting processing for species: {species_name}**")
        print("="*50)
        
        species_df = self.filter_species_data(species_name)
        print(f"Processing {len(species_df)} observations for {species_name}...")
        
        success_count = 0
        for index, row in species_df.iterrows():
            url = row['url']
            photo_id = str(row['photo_id'])
            
            if self.download_image(url, photo_id, row, min_sleep, max_sleep):
                success_count += 1
        
        print(f"\nCompleted {species_name}: {success_count} images downloaded")
        return success_count
    
    def process_species_list(
        self,
        species_list: List[str],
        skip_processed: bool = True,
        processed_csv_path: Optional[str] = None,
        min_sleep: float = 0.5,
        max_sleep: float = 2.0
    ) -> Dict[str, int]:
        """
        Process multiple species.
        
        Args:
            species_list: List of species to process
            skip_processed: Whether to skip already processed species
            processed_csv_path: Path to CSV with processed species
            min_sleep: Minimum sleep time between downloads
            max_sleep: Maximum sleep time between downloads
            
        Returns:
            Dictionary with species names and download counts
        """
        processed_species = set()
        if skip_processed:
            processed_species = self.get_processed_species(processed_csv_path)
            if processed_species:
                print(f"Found {len(processed_species)} already processed species:")
                for sp in processed_species:
                    print(f"  - {sp}")
        
        results = {}
        for species_name in species_list:
            if species_name in processed_species:
                print(f"\n⏭️ Skipping {species_name} (already processed)")
                results[species_name] = 0
                continue
            
            count = self.process_species(species_name, min_sleep, max_sleep)
            results[species_name] = count
        
        return results
    
    def save_metadata(self, append: bool = False) -> None:
        """
        Save metadata to CSV file.
        
        Args:
            append: Whether to append to existing file
        """
        if not self.metadata_list:
            print("\nNo new images were downloaded, so no metadata file was updated.")
            return
        
        master_metadata_df = pd.DataFrame(self.metadata_list)
        
        if append and os.path.exists(self.master_metadata_file):
            try:
                existing_df = pd.read_csv(self.master_metadata_file)
                master_metadata_df = pd.concat([existing_df, master_metadata_df], ignore_index=True)
                print(f"Appending to existing metadata file...")
            except Exception as e:
                print(f"Warning: Could not read existing metadata file: {e}")
                print(f"Creating new metadata file instead...")
        
        master_metadata_df.to_csv(self.master_metadata_file, index=False)
        print(f"\nSuccessfully saved metadata to: **{self.master_metadata_file}**")
        print(f"Total records: {len(master_metadata_df)}")


# Convenience function for quick usage
def download_species_images(
    csv_path: str,
    species_list: List[str],
    base_output_dir: str = "PlantNet_Batch_Images",
    skip_processed: bool = True,
    processed_csv_path: Optional[str] = None,
    min_sleep: float = 0.5,
    max_sleep: float = 2.0,
    append_metadata: bool = False
) -> Dict[str, int]:
    """
    Convenience function to download images for multiple species.
    
    Args:
        csv_path: Path to the CSV file containing observations
        species_list: List of species to process
        base_output_dir: Directory where images will be saved
        skip_processed: Whether to skip already processed species
        processed_csv_path: Path to CSV with processed species
        min_sleep: Minimum sleep time between downloads
        max_sleep: Maximum sleep time between downloads
        append_metadata: Whether to append to existing metadata file
        
    Returns:
        Dictionary with species names and download counts
    """
    downloader = FlickrImageDownloader(csv_path, base_output_dir)
    
    results = downloader.process_species_list(
        species_list,
        skip_processed=skip_processed,
        processed_csv_path=processed_csv_path,
        min_sleep=min_sleep,
        max_sleep=max_sleep
    )
    
    downloader.save_metadata(append=append_metadata)
    
    return results


def main():
    """
    Main function to run the script standalone with default configuration.
    """
    # Configuration - modify these as needed
    CSV_PATH = 'deduplicated_geocoded_flickr_2004-now.csv'
    BASE_OUTPUT_DIR = "PlantNet_Batch_Images"
    
    # List of species to process
    species_list = [
        'Heracleum mantegazzianum', 
        'Ailanthus altissima', 
        'Fallopia japonica', 
        'Impatiens glandulifera', 
        'Rhododendron ponticum', 
        'Acacia saligna'
    ]
    
    # Optional: Path to CSV with already processed species
    PROCESSED_CSV_PATH = 'processed_Reynoutria_japonica_example.csv'
    
    # Sleep parameters (seconds)
    MIN_SLEEP = 0.5
    MAX_SLEEP = 2.0
    
    print("="*60)
    print("Flickr Plant Image Downloader")
    print("="*60)
    
    # Run the download process
    results = download_species_images(
        csv_path=CSV_PATH,
        species_list=species_list,
        base_output_dir=BASE_OUTPUT_DIR,
        skip_processed=True,
        processed_csv_path=PROCESSED_CSV_PATH,
        min_sleep=MIN_SLEEP,
        max_sleep=MAX_SLEEP,
        append_metadata=True
    )
    
    # Print summary
    print("\n" + "="*60)
    print("Download Summary")
    print("="*60)
    total_downloads = 0
    for species, count in results.items():
        status = "✅" if count > 0 else "⏭️"
        print(f"{status} {species}: {count} images")
        total_downloads += count
    
    print(f"\nTotal new images downloaded: {total_downloads}")
    print("\n--- Script finished ---")


if __name__ == "__main__":
    main()