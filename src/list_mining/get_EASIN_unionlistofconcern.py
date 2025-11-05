#load dependencies
import requests
import csv
import os

def fetch_and_process_easin_data(url, output_file):
    """
    Fetches data from the EASIN API, processes it, and saves it to a CSV file.
    Includes the unique EASINID (R identifier) in the output.

    Args:
        url (str): The API endpoint URL.
        output_file (str): The path to the output CSV file.
    """
    # Prepare the CSV header
    # NOTE: The unique ID is correctly identified as 'EASINID'
    header = ['EASINID', 'Scientific Name', 'Label', 'All Names']

    try:
        # Make a GET request to the API
        response = requests.get(url)
        response.raise_for_status()

        # Open the CSV file for writing
        with open(output_file, mode='w', newline='', encoding='utf-8') as file:
            writer = csv.writer(file)
            writer.writerow(header)  # Write the header row
            
            # Parse the JSON response
            data = response.json()
            
            # Iterate over the list of species
            for species_item in data:
                # 1. Get the unique R identifier (EASINID)
                easin_id = species_item.get('EASINID', 'N/A')
                scientific_name = species_item.get('Name', 'N/A')
                
                # 2. Extract and write Common Names (using or [] to prevent NoneType error)
                common_names_data = species_item.get('CommonNames') or []
                
                common_names = [
                    common_name.get('Name', 'N/A') 
                    for common_name in common_names_data 
                ]
                for common_name in common_names:
                    # Write row: [EASINID, Scientific Name, Label, Name]
                    writer.writerow([easin_id, scientific_name, 'Common Name', common_name])

                # 3. Extract and write Synonyms (using or [] to prevent NoneType error)
                synonyms_data = species_item.get('Synonyms') or []
                
                synonym_names = [
                    synonym.get('Synonym', 'N/A') 
                    for synonym in synonyms_data 
                ]
                for synonym in synonym_names:
                    # Write row: [EASINID, Scientific Name, Label, Name]
                    writer.writerow([easin_id, scientific_name, 'Synonym', synonym])
        
        print(f"Data successfully saved to {output_file}")
    
    except requests.exceptions.RequestException as e:
        print(f"Error: Unable to fetch data from the API ({e})")
    except (IOError, csv.Error) as e:
        print(f"Error: Unable to write to the CSV file ({e})")
    except Exception as e:
        print(f"An unexpected error occurred: {e}")

# --- Main Script Execution ---
if __name__ == "__main__":
    url = "https://easin.jrc.ec.europa.eu/apixg/catxg/euconcern"
    output_file = "EASIN_species_names_synonyms_with_id.csv"
    fetch_and_process_easin_data(url, output_file)