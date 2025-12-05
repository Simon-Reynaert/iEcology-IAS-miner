import pandas as pd
from reverse_geocoder import search as rg_search
import pycountry
from tqdm import tqdm
import math

# Define the set of European countries (EU, Schengen, EFTA) using ISO 3166-1 alpha-2 codes
# This includes EU member states, EFTA countries, and some Balkan countries
EU_SCHENGEN_EFTA_COUNTRIES = {
    'AT', 'BE', 'BG', 'HR', 'CY', 'CZ', 'DK', 'EE', 'FI', 'FR',  # EU countries
    'DE', 'GR', 'HU', 'IE', 'IT', 'LV', 'LT', 'LU', 'MT', 'NL',
    'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE',
    'IS', 'LI', 'NO', 'CH', 'GB', 'MK', 'BA', 'RS', 'AL', 'ME', 'XK'  # EFTA + others
}

def cc_to_name(cc):
    """
    Convert a two-letter country code to its full country name.
    
    Parameters
    ----------
    cc : str
        ISO 3166-1 alpha-2 country code (e.g., 'DE', 'FR')
    
    Returns
    -------
    str
        Full country name if found, otherwise returns the original code
    """
    country = pycountry.countries.get(alpha_2=cc)
    return country.name if country else cc

def haversine(lat1, lon1, lat2, lon2):
    """
    Calculate the great-circle distance between two points on Earth.
    Uses the Haversine formula to compute distance in meters.
    
    Parameters
    ----------
    lat1, lon1 : float
        Latitude and longitude of the first point in decimal degrees
    lat2, lon2 : float
        Latitude and longitude of the second point in decimal degrees
    
    Returns
    -------
    float
        Distance between the two points in meters
    """
    R = 6371000  # Earth's radius in meters
    
    # Convert latitude and longitude from degrees to radians
    phi1, phi2 = math.radians(lat1), math.radians(lat2)
    delta_phi = math.radians(lat2 - lat1)
    delta_lambda = math.radians(lon2 - lon1)
    
    # Haversine formula
    a = math.sin(delta_phi / 2.0) ** 2 + \
        math.cos(phi1) * math.cos(phi2) * math.sin(delta_lambda / 2.0) ** 2
    c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))
    
    return R * c

def process_flickr_data(
    input_file='flickr_species_observations_eu_combined_latin_normtag_2004-now.csv',
    output_file='deduplicated_geocoded_flickr_2004-now_TEST2.csv',
    distance_threshold_meters=100.0
):
    """
    Deduplicate Flickr observations by removing photos that are similar in tags,
    location, and time. After deduplication, geocode the remaining observations
    and filter to only include European locations.
    
    The deduplication algorithm marks two photos as duplicates if:
    - They were taken on the same day
    - Their tags have ≥70% similarity (Jaccard index)
    - They are within the specified distance threshold
    - They were taken within 1 hour of each other
    
    Parameters
    ----------
    input_file : str
        Path to the input CSV file containing Flickr observations.
        Expected columns: latitude, longitude, tags, date_taken
    output_file : str
        Path where the deduplicated and geocoded CSV will be saved
    distance_threshold_meters : float
        Maximum distance between photos (in meters) to consider them duplicates.
        Default is 100 meters.
    
    Returns
    -------
    None
        Saves the processed data to the output file and prints summary statistics
    """
    # Read the input CSV file
    df = pd.read_csv(input_file)
    
    # Remove rows with missing coordinates (required for distance calculations)
    df = df.dropna(subset=['latitude', 'longitude'])

    # Prepare tag data for similarity comparisons
    df['tags'] = df['tags'].fillna('')  # Replace NaN tags with empty strings
    df['tag_list'] = df['tags'].str.lower().str.split()  # Convert to lowercase and split into lists
    # Create sets of tags, excluding tags starting with 'ngid' (likely internal IDs)
    df['tag_set'] = df['tag_list'].apply(lambda tags: set(t for t in tags if not t.startswith('ngid')))
    
    # Parse timestamps and extract date for grouping
    df['timestamp'] = pd.to_datetime(df['date_taken'], errors='coerce')
    df['date_only'] = df['timestamp'].dt.date.astype(str)

    # List to store indices of rows to keep after deduplication
    keep_indices = []

    # Deduplication process with progress bar
    total_rows = len(df)
    with tqdm(total=total_rows, desc="Deduplicating rows") as pbar:
        # Process each day separately to reduce comparison complexity
        for date, group in df.groupby('date_only'):
            seen_local = set()  # Track indices marked as duplicates within this date group
            
            # Extract data for this group into lists for faster access
            idxs = group.index.tolist()
            tag_sets = group['tag_set'].tolist()
            latitudes = group['latitude'].tolist()
            longitudes = group['longitude'].tolist()
            timestamps = group['timestamp'].tolist()

            # Compare each photo with all subsequent photos in the same day
            for pos, i in enumerate(idxs):
                pbar.update(1)
                
                # Skip if this photo was already marked as a duplicate
                if i in seen_local:
                    continue
                
                # Keep this photo (it's not a duplicate)
                keep_indices.append(i)
                
                # Get properties of current photo
                tags_i = tag_sets[pos]
                lat_i = latitudes[pos]
                lon_i = longitudes[pos]
                time_i = timestamps[pos]

                # Compare with all subsequent photos in the group
                for j_pos in range(pos + 1, len(idxs)):
                    j = idxs[j_pos]
                    
                    # Skip if already marked as duplicate
                    if j in seen_local:
                        continue
                    
                    # Get properties of comparison photo
                    tags_j = tag_sets[j_pos]
                    lat_j = latitudes[j_pos]
                    lon_j = longitudes[j_pos]
                    time_j = timestamps[j_pos]

                    # Calculate Jaccard similarity for tags (intersection / union)
                    union = tags_i | tags_j
                    similarity = len(tags_i & tags_j) / len(union) if len(union) > 0 else 0
                    
                    # Calculate geographic distance between photos
                    distance = haversine(lat_i, lon_i, lat_j, lon_j)
                    
                    # Calculate time difference in hours
                    time_diff = abs((time_i - time_j).total_seconds()) / 3600.0

                    # Mark as duplicate if all conditions are met
                    if similarity >= 0.7 and distance < distance_threshold_meters and time_diff <= 1.0:
                        seen_local.add(j)

    # Create filtered dataframe with only kept indices
    df_filtered = (
        df.loc[keep_indices]
          .drop(columns=['tag_list', 'tag_set', 'date_only'])  # Remove temporary columns
          .reset_index(drop=True)  # Reset index to be sequential
    )

    # Reverse geocode all remaining coordinates to determine country
    # Convert coordinates to tuple format required by reverse_geocoder
    coords = tuple(zip(df_filtered['latitude'], df_filtered['longitude']))
    rg_results = rg_search(coords, mode=1, verbose=False)

    # Add detected country code and full country name to dataframe
    df_filtered['detected_cc'] = [r['cc'] for r in rg_results]
    df_filtered['detected_country'] = df_filtered['detected_cc'].apply(cc_to_name)

    # Filter to only include observations from European countries
    df_europe = df_filtered[df_filtered['detected_cc'].isin(EU_SCHENGEN_EFTA_COUNTRIES)].reset_index(drop=True)

    # Save the final deduplicated and filtered dataset
    df_europe.to_csv(output_file, index=False)
    print(f"Deduplicated & geocoded European data saved to: {output_file}")
    print(f"Number of rows in final CSV: {len(df_europe)}")

# Entry point when script is run directly (not when imported)
if __name__ == "__main__":
    # Support for multiprocessing on Windows
    import multiprocessing
    multiprocessing.freeze_support()
    
    # Run with default parameters
    process_flickr_data()