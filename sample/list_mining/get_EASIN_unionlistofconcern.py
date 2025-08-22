#import dependencies
import requests
import csv

# URL/endpoint of the API
url = "https://easin.jrc.ec.europa.eu/apixg/catxg/euconcern"

# Make a GET request to the API
response = requests.get(url)

# Prepare the CSV output file
output_file = "EASIN_species_names_synonyms.csv"
header = ['Scientific Name', 'Label', 'All Names']

# Open the CSV file for writing
with open(output_file, mode='w', newline='', encoding='utf-8') as file:
    writer = csv.writer(file)
    writer.writerow(header)  # Write the header row

    # Check the status code of the response
    if response.status_code == 200:
        # Parse the JSON response
        data = response.json()
        
        # Iterate over the list of species
        for species_item in data:
            # Get the scientific name (direct 'Name' field)
            scientific_name = species_item.get('Name', 'N/A')  # Direct 'Name' field
            
            # Extract common names, writing each common name with a label
            common_names = [common_name.get('Name', 'N/A') for common_name in species_item.get('CommonNames', []) or []]
            for common_name in common_names:
                writer.writerow([scientific_name, 'Common Name', common_name])

            # Extract synonyms, writing each synonym with a label
            synonyms = species_item.get('Synonyms', [])
            # If synonyms is not a list or is None, treat it as an empty list
            if not isinstance(synonyms, list):
                synonyms = []
            synonym_names = [synonym.get('Synonym', 'N/A') for synonym in synonyms]
            for synonym in synonym_names:
                writer.writerow([scientific_name, 'Synonym', synonym])
    
        print(f"Data successfully saved to {output_file}")
    else:
        print(f"Error: Unable to fetch data from the API (Status code: {response.status_code})")
