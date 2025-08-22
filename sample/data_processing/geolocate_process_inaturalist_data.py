#load dependencies
import os
import glob
import pandas as pd
import numpy as np
import reverse_geocoder as rg
from datetime import datetime

# Define the folder containing your CSV files
folder = "species_inat_observations_onlycasual"
csv_files = glob.glob(os.path.join(folder, "*.csv"))

##############################
# GEOLOCATE iNAturalist DATA #
##############################

# Process each CSV file
for file in csv_files:
    print(f"Processing file: {file}")
    df = pd.read_csv(file)

    # Ensure the 'Country' column exists
    if 'Country' not in df.columns:
        df['Country'] = ""
    
    # Function to lookup country based on latitude and longitude if Country is missing
    def lookup_country(row):
        # If the Country is already present, return it
        if pd.notna(row['Country']) and row['Country'] != "":
            return row['Country']
        
        lat = row['latitude']
        lon = row['longitude']
        
        # Check if latitude or longitude is missing or not finite
        if pd.isna(lat) or pd.isna(lon) or not np.isfinite(lat) or not np.isfinite(lon):
            return ""
        
        coord = (lat, lon)
        try:
            # reverse_geocoder.search expects a list of coordinate tuples
            result = rg.search([coord], mode=1)
            return result[0]['cc']
        except Exception as e:
            print(f"Error processing coordinates {coord}: {e}")
            return ""

    # Apply the function to each row to populate the Country column
    df['Country'] = df.apply(lookup_country, axis=1)

    # Create a new file name by replacing '_observations.csv' with '_geolocated.csv'
    base_name = os.path.basename(file)
    new_name = base_name.replace('_observations.csv', '_geolocated.csv')
    output_path = os.path.join(os.path.dirname(file), new_name)

    # Save the updated CSV file with the new name
    df.to_csv(output_path, index=False)
    print(f"Updated file saved as: {output_path}")

#-----------------------------#
##JOIN DATA FRAMES WTIH COUNT##
#-----------------------------#

# Define the folder containing your geolocated CSV files
folder = "species_inat_observations_onlycasual"

# Only search for CSV files with '_geolocated.csv' in their name
csv_files = glob.glob(os.path.join(folder, "*_geolocated.csv"))

df_list = []

# Process each geolocated CSV file
for file in csv_files:
    print(f"Reading file: {file}")
    df = pd.read_csv(file)
    
    # Extract the scientific name from the filename
    # e.g., "Acacia_saligna_geolocated.csv" -> "Acacia saligna"
    base_name = os.path.basename(file)
    species_part = base_name.replace("_geolocated.csv", "")
    scientific_name = species_part.replace("_", " ")
    
    # Add the "Scientific Name" column with the extracted name for all rows
    df["Scientific Name"] = scientific_name

    # Ensure the 'observed_on' column is parsed as datetime
    df['observed_on'] = pd.to_datetime(df['observed_on'], errors='coerce')
    
    # Keep only rows with a valid 'observed_on' date
    df = df.dropna(subset=['observed_on'])
    df_list.append(df)

# Concatenate all data into one DataFrame
if df_list:
    combined_df = pd.concat(df_list, ignore_index=True)
else:
    print("No geolocated CSV files found.")
    exit()

# Create a date range from 2022-01-01 to today's date
start_date = pd.to_datetime("2016-01-01")
end_date = pd.to_datetime(datetime.today().strftime('%Y-%m-%d'))
date_range = pd.date_range(start_date, end_date, freq='D')
date_columns = [d.strftime('%Y-%m-%d') for d in date_range]

# Create a new column for date strings to pivot on
combined_df['date_str'] = combined_df['observed_on'].dt.strftime('%Y-%m-%d')

# Group by 'Scientific Name', 'Country', and 'date_str' to count observations
grouped = combined_df.groupby(['Scientific Name', 'Country', 'date_str']).size().reset_index(name='count')

# Pivot the table so that each date becomes a column; fill missing values with 0
pivot_df = grouped.pivot_table(index=['Scientific Name', 'Country'],
                               columns='date_str',
                               values='count',
                               fill_value=0)

# Ensure all dates in our range appear as columns. If a date is missing, add it with zeros.
for date in date_columns:
    if date not in pivot_df.columns:
        pivot_df[date] = 0

# Corrected: Sort the columns based on their actual date values
# First, get the date columns (excluding 'Scientific Name' and 'Country' if they somehow got in there)
current_date_cols = [col for col in pivot_df.columns if col not in ['Scientific Name', 'Country']]
# Convert them to datetime objects for proper sorting, then back to string for column indexing
sorted_date_cols = sorted(current_date_cols, key=lambda x: datetime.strptime(x, '%Y-%m-%d'))

# Combine the fixed index columns with the sorted date columns
fixed_columns = ['Scientific Name', 'Country'] # These should be at the beginning
# We need to make sure these columns exist in pivot_df before indexing
# In this specific case, 'Scientific Name' and 'Country' are part of the index before reset_index()
# So, we'll sort columns *after* reset_index if we want them as regular columns at the start.
# For now, let's just reorder the date columns.
pivot_df = pivot_df[sorted_date_cols]


# Reset the index so that 'Scientific Name' and 'Country' become columns again
final_df = pivot_df.reset_index()

# Now, ensure 'Scientific Name' and 'Country' are at the very beginning of the DataFrame
# Get all columns and reorder them
all_columns = final_df.columns.tolist()
# Remove 'Scientific Name' and 'Country' from their current positions if they were already there from reset_index
for col in ['Scientific Name', 'Country']:
    if col in all_columns:
        all_columns.remove(col)
# Prepend them to the list of columns
final_column_order = ['Scientific Name', 'Country'] + all_columns
final_df = final_df[final_column_order]


# Optionally sort the DataFrame by 'Scientific Name' and 'Country'
final_df = final_df.sort_values(by=['Scientific Name', 'Country'])

# Save the final merged DataFrame to a CSV file
output_file = "species_country_observations_inat_2016_present.csv"
final_df.to_csv(output_file, index=False)
print(f"Final merged CSV saved as: {output_file}")
