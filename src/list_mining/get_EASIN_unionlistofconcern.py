#load dependencies
import requests
import csv
import os

def fetch_and_process_easin_data(url, output_file):
    """
    Fetches data from the EASIN API, processes it, and saves it to a CSV file.

    Args:
        url (str): The API endpoint URL.
        output_file (str): The path to the output CSV file.
    """
    # Prepare the CSV header
    header = ['Scientific Name', 'Label', 'All Names']

    try:
        # Make a GET request to the API
        response = requests.get(url)
        response.raise_for_status()  # This will raise an HTTPError if the status code is bad

        # Open the CSV file for writing
        with open(output_file, mode='w', newline='', encoding='utf-8') as file:
            writer = csv.writer(file)
            writer.writerow(header)  # Write the header row
            
            # Parse the JSON response
            data = response.json()
            
            # Iterate over the list of species
            for species_item in data:
                # Get the scientific name (direct 'Name' field)
                scientific_name = species_item.get('Name', 'N/A')
                
                # Extract common names and write each one with a label
                common_names = [common_name.get('Name', 'N/A') for common_name in species_item.get('CommonNames', []) or []]
                for common_name in common_names:
                    writer.writerow([scientific_name, 'Common Name', common_name])

                # Extract synonyms and write each one with a label
                synonyms = species_item.get('Synonyms', [])
                if not isinstance(synonyms, list):
                    synonyms = []
                synonym_names = [synonym.get('Synonym', 'N/A') for synonym in synonyms]
                for synonym in synonym_names:
                    writer.writerow([scientific_name, 'Synonym', synonym])
        
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
    output_file = "EASIN_species_names_synonyms.csv"
    fetch_and_process_easin_data(url, output_file)
