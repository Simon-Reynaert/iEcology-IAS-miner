import pandas as pd

# Load the first CSV (the one to be expanded)
first_csv_path = 'UnionList_extended_synonyms.csv'  # Path to the first .csv file
first_df = pd.read_csv(first_csv_path)

# Load the second CSV (the one containing synonyms to check)
second_csv_path = 'species_synonyms.csv'  # Path to the second .csv file
second_df = pd.read_csv(second_csv_path)

# Initialize a list to hold new rows to be added to the first CSV
new_rows = []

# Iterate over each row in the second dataframe
for _, row in second_df.iterrows():
    species = row['Scientific Name']
    synonym = row['Synonym']
    
    # Check if this synonym is already in the first CSV
    if not ((first_df['Scientific Name'] == species) & (first_df['Name'] == synonym)).any():
        # If not, append a new row with the species and synonym
        new_rows.append([species, synonym, 'Synonym', row['Language']])

# Convert the new rows to a DataFrame
new_df = pd.DataFrame(new_rows, columns=['Scientific Name', 'Name', 'Type', 'Language'])

# Append the new rows to the first CSV
expanded_df = pd.concat([first_df, new_df], ignore_index=True)

# Save the expanded dataframe back to a CSV
expanded_df.to_csv('UnionList_Wiki_GBIF_TOL_synonyms.csv', index=False)

print("New rows added and saved to 'expanded_first_file.csv'")
