"""
iNaturalist Data Processing Module

This module processes iNaturalist observation data by adding geolocation information
and creating time-series datasets. It performs three main operations:
1. Geolocates observations using latitude/longitude coordinates
2. Aggregates observations by species, country, and date
3. Creates a pivoted time-series DataFrame with dates as columns

The output is suitable for temporal analysis of species observations across countries.
"""

import os
import glob
import pandas as pd
import numpy as np
import reverse_geocoder as rg
from datetime import datetime


def _lookup_country(row):
    """
    Lookup country code from latitude/longitude using reverse geocoding.
    
    This helper function performs reverse geocoding to determine the country code
    for a given observation. If a country is already specified, it returns that value.
    
    Parameters
    ----------
    row : pd.Series
        A pandas Series representing a row from the DataFrame.
        Expected to contain 'Country', 'latitude', and 'longitude' fields.
    
    Returns
    -------
    str
        ISO 3166-1 alpha-2 country code (e.g., 'US', 'DE', 'FR').
        Returns empty string if geocoding fails or coordinates are invalid.
    
    Notes
    -----
    - If 'Country' field is already populated, it returns that value unchanged
    - Handles missing or invalid coordinates gracefully
    - Uses reverse_geocoder library in mode 1 (single-threaded) for reliability
    """
    # Return existing country value if already populated
    if pd.notna(row['Country']) and row['Country'] != "":
        return row['Country']
    
    # Extract coordinates from the row
    lat, lon = row['latitude'], row['longitude']
    
    # Validate coordinates: check for missing values and ensure they're finite numbers
    if pd.isna(lat) or pd.isna(lon) or not np.isfinite(lat) or not np.isfinite(lon):
        return ""
    
    try:
        # Perform reverse geocoding lookup
        # mode=1 ensures single-threaded operation for stability
        result = rg.search([(lat, lon)], mode=1)
        return result[0]['cc']  # Extract country code from first result
    except Exception as e:
        # Log errors but continue processing (fail gracefully)
        print(f"Error processing coordinates {(lat, lon)}: {e}")
        return ""


def geolocate_csv_file(input_file, output_file=None):
    """
    Add a 'Country' column to a single CSV file based on latitude/longitude coordinates.
    
    This function reads an iNaturalist observation CSV, performs reverse geocoding
    on each row to determine the country, and saves the result with an added
    'Country' column.
    
    Parameters
    ----------
    input_file : str
        Path to the input CSV file containing observation data.
        Must include 'latitude' and 'longitude' columns.
    output_file : str, optional
        Path where the geolocated CSV should be saved.
        If None, automatically generates filename by replacing '_observations.csv' 
        with '_geolocated.csv' in the same directory as input_file.
    
    Returns
    -------
    pd.DataFrame
        DataFrame with added 'Country' column containing ISO country codes
    
    Examples
    --------
    >>> df = geolocate_csv_file('species_observations.csv')
    Geolocated CSV saved to: species_geolocated.csv
    
    >>> df = geolocate_csv_file('input.csv', 'output/geolocated.csv')
    Geolocated CSV saved to: output/geolocated.csv
    """
    # Read the input CSV file
    df = pd.read_csv(input_file)
    
    # Add 'Country' column if it doesn't exist
    if 'Country' not in df.columns:
        df['Country'] = ""
    
    # Apply reverse geocoding to each row
    df['Country'] = df.apply(_lookup_country, axis=1)
    
    # Generate output filename if not specified
    if output_file is None:
        output_file = os.path.basename(input_file).replace('_observations.csv', '_geolocated.csv')
        output_file = os.path.join(os.path.dirname(input_file), output_file)
    
    # Save the geolocated data
    df.to_csv(output_file, index=False)
    print(f"Geolocated CSV saved to: {output_file}")
    return df


def geolocate_folder(input_folder, output_folder=None):
    """
    Geolocate all CSV files in a specified folder.
    
    Processes multiple iNaturalist observation CSV files in batch, adding country
    information to each file through reverse geocoding.
    
    Parameters
    ----------
    input_folder : str
        Path to folder containing CSV files to process.
        All .csv files in this folder will be processed.
    output_folder : str, optional
        Folder where geolocated CSVs should be saved.
        If None, files are saved in the same folder as the input files.
        The output folder will be created if it doesn't exist.
    
    Returns
    -------
    list of pd.DataFrame
        List containing geolocated DataFrames for each processed file
    
    Examples
    --------
    >>> dfs = geolocate_folder('raw_data/')
    Geolocated CSV saved to: raw_data/species1_geolocated.csv
    Geolocated CSV saved to: raw_data/species2_geolocated.csv
    
    >>> dfs = geolocate_folder('raw_data/', 'processed_data/')
    Geolocated CSV saved to: processed_data/species1_geolocated.csv
    """
    # Find all CSV files in the input folder
    csv_files = glob.glob(os.path.join(input_folder, "*.csv"))
    geolocated_dfs = []
    
    # Process each CSV file
    for file in csv_files:
        output_file = None
        if output_folder:
            # Create output folder if it doesn't exist
            os.makedirs(output_folder, exist_ok=True)
            # Generate output filename in the output folder
            output_file = os.path.join(
                output_folder, 
                os.path.basename(file).replace('_observations.csv', '_geolocated.csv')
            )
        
        # Geolocate the file and store the resulting DataFrame
        df_geo = geolocate_csv_file(file, output_file)
        geolocated_dfs.append(df_geo)
    
    return geolocated_dfs


def join_and_pivot_geolocated(input_folder, start_date=None, end_date=None):
    """
    Join all geolocated CSVs and pivot into a time-series format by date.
    
    This function combines multiple geolocated species observation files into a single
    DataFrame with a time-series structure. Each row represents a species-country 
    combination, and each column represents a date, with cell values showing the
    count of observations.
    
    Parameters
    ----------
    input_folder : str
        Folder containing geolocated CSV files (files ending in '_geolocated.csv').
    start_date : str or pd.Timestamp, optional
        Start date for the time series (e.g., '2016-01-01').
        If None, uses the earliest observation date in the data.
    end_date : str or pd.Timestamp, optional
        End date for the time series (e.g., '2024-12-31').
        If None, uses the latest observation date in the data.
    
    Returns
    -------
    pd.DataFrame
        Pivoted DataFrame with:
        - Index: ['Scientific Name', 'Country'] (species-country combinations)
        - Columns: Individual dates as 'YYYY-MM-DD' strings
        - Values: Count of observations for that species/country/date combination
        - Missing dates are filled with 0
    
    Notes
    -----
    - Species names are extracted from filenames by replacing '_geolocated.csv'
      and converting underscores to spaces
    - Only rows with valid 'observed_on' dates are included
    - Date columns are sorted chronologically
    - All dates in the specified range are included, even if no observations exist
    
    Examples
    --------
    >>> df = join_and_pivot_geolocated('geolocated_data/')
    >>> print(df.head())
    Scientific Name    Country  2016-01-01  2016-01-02  2016-01-03
    Species alpha      US       5           3           0
    Species alpha      DE       2           1           4
    
    >>> df = join_and_pivot_geolocated('data/', '2020-01-01', '2020-12-31')
    """
    # Find all geolocated CSV files in the folder
    csv_files = glob.glob(os.path.join(input_folder, "*_geolocated.csv"))
    if not csv_files:
        print("No geolocated CSV files found.")
        return pd.DataFrame()

    df_list = []
    for file in csv_files:
        # Read each geolocated CSV
        df = pd.read_csv(file)
        
        # Extract species name from filename
        # Example: "species_name_geolocated.csv" -> "species name"
        species_name = os.path.basename(file).replace("_geolocated.csv", "").replace("_", " ")
        df['Scientific Name'] = species_name
        
        # Parse observation dates and remove invalid dates
        df['observed_on'] = pd.to_datetime(df['observed_on'], errors='coerce')
        df = df.dropna(subset=['observed_on'])
        df_list.append(df)

    # Combine all species data into a single DataFrame
    combined_df = pd.concat(df_list, ignore_index=True)

    # Determine date range for the pivot table
    # Use provided dates or infer from data
    start_date = pd.to_datetime(start_date) if start_date else combined_df['observed_on'].min()
    end_date = pd.to_datetime(end_date) if end_date else combined_df['observed_on'].max()
    
    # Create complete date range (ensures all dates are represented, even with 0 observations)
    date_range = pd.date_range(start=start_date, end=end_date, freq='D')
    date_columns = [d.strftime('%Y-%m-%d') for d in date_range]

    # Convert datetime to string format for grouping
    combined_df['date_str'] = combined_df['observed_on'].dt.strftime('%Y-%m-%d')
    
    # Group by species, country, and date, then count observations
    grouped = combined_df.groupby(['Scientific Name', 'Country', 'date_str']).size().reset_index(name='count')
    
    # Pivot the data: rows = species+country, columns = dates, values = counts
    pivot_df = grouped.pivot_table(
        index=['Scientific Name', 'Country'],
        columns='date_str',
        values='count',
        fill_value=0  # Fill missing combinations with 0
    ).reindex(columns=date_columns, fill_value=0)  # Ensure all dates in range are present

    # Reset index to make 'Scientific Name' and 'Country' regular columns
    final_df = pivot_df.reset_index()
    
    # Organize columns: static columns first, then sorted date columns
    static_cols = ['Scientific Name', 'Country']
    date_cols_sorted = sorted([c for c in final_df.columns if c not in static_cols])
    final_df = final_df[static_cols + date_cols_sorted].sort_values(
        by=['Scientific Name', 'Country']
    ).reset_index(drop=True)
    
    return final_df


def process_inat_data(input_folder, output_file, start_date=None, end_date=None, geolocated_folder=None):
    """
    Complete workflow to process iNaturalist observation data.
    
    This is the main entry point function that orchestrates the entire processing
    pipeline:
    1. Geolocate all CSV files in the input folder
    2. Join and pivot the geolocated data into time-series format
    3. Save the final merged dataset to a CSV file
    
    Parameters
    ----------
    input_folder : str
        Folder containing raw iNaturalist observation CSV files.
        Files should have 'latitude', 'longitude', and 'observed_on' columns.
    output_file : str
        Path where the final merged CSV should be saved.
        Example: 'species_country_observations_2016_present.csv'
    start_date : str or pd.Timestamp, optional
        Start date for the time series (e.g., '2016-01-01').
        If None, uses earliest date in the data.
    end_date : str or pd.Timestamp, optional
        End date for the time series (e.g., '2024-12-31').
        If None, uses latest date in the data.
    geolocated_folder : str, optional
        Folder where intermediate geolocated CSVs should be saved.
        If None, geolocated files are saved in the input_folder.
        Useful for organizing intermediate processing files separately.
    
    Returns
    -------
    pd.DataFrame
        Final pivoted DataFrame with time-series structure:
        - Rows: species-country combinations
        - Columns: dates
        - Values: observation counts
    
    Examples
    --------
    >>> df = process_inat_data(
    ...     'raw_observations/', 
    ...     'final_output.csv',
    ...     start_date='2020-01-01',
    ...     end_date='2023-12-31'
    ... )
    Geolocated CSV saved to: raw_observations/species1_geolocated.csv
    Geolocated CSV saved to: raw_observations/species2_geolocated.csv
    Final merged CSV saved as: final_output.csv
    
    >>> # Save intermediate files to separate folder
    >>> df = process_inat_data(
    ...     'raw/', 
    ...     'output.csv',
    ...     geolocated_folder='intermediate/'
    ... )
    """
    # Step 1: Geolocate all CSV files in the input folder
    geolocate_folder(input_folder, geolocated_folder)
    
    # Step 2: Join and pivot the geolocated data
    # Use geolocated_folder if specified, otherwise use input_folder
    final_df = join_and_pivot_geolocated(
        geolocated_folder or input_folder, 
        start_date, 
        end_date
    )
    
    # Step 3: Save the final dataset if it's not empty
    if not final_df.empty:
        final_df.to_csv(output_file, index=False)
        print(f"Final merged CSV saved as: {output_file}")
    
    return final_df


# Entry point for running as a standalone script
if __name__ == "__main__":
    # Configuration for the processing pipeline
    input_folder = "species_inat_observations_nonresearch"  # Folder with raw observation CSVs
    output_file = "species_country_observations_inat_2016_present.csv"  # Final output file
    geolocated_folder = None  # Optional: specify separate folder for intermediate geolocated CSVs
    
    # Run the complete processing pipeline
    process_inat_data(input_folder, output_file, geolocated_folder=geolocated_folder)