# load dependencies
import sys
import os
import pandas as pd
import requests
from bs4 import BeautifulSoup
from typing import List, Dict, Any
from dotenv import load_dotenv
from tqdm.auto import tqdm

# Load environment variables from .env file
load_dotenv()

# Define the official EU languages (ISO 639-1 codes)
EU_LANGUAGES = [
    "bg", "cs", "da", "de", "el", "en", "es", "et", "fi", "fr", "ga", "hr",
    "hu", "it", "lt", "lv", "mt", "nl", "pl", "pt", "ro", "sk", "sl", "sv",
    "is", "no",
    "eu", "gl", "ca", "sq", "bs", "mk", "sr", "tr",
    "cy"
]

# Get the user-agent string from an environment variable
WIKI_USER_AGENT = os.getenv("WIKI_USER_AGENT")

# Set a default user-agent if the environment variable is not set
if not WIKI_USER_AGENT:
    print("Warning: WIKI_USER_AGENT environment variable not set. Using a default user-agent - MIGHT GET BLOCKED.")
    WIKI_USER_AGENT = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3'

# Initialize tqdm for pandas
tqdm.pandas()

def fetch_webpage_content(url: str) -> requests.Response:
    """
    Fetches the content of a webpage with a User-Agent header and returns the response object.
    """
    headers = {'User-Agent': WIKI_USER_AGENT}
    try:
        response = requests.get(url, headers=headers)
        response.raise_for_status()
        return response
    except requests.exceptions.RequestException as e:
        print(f"Error fetching URL {url}: {e}")
        return None

def extract_scientific_names(html_content: str) -> List[str]:
    """
    Parses HTML content to find a specific table and extracts scientific names.
    """
    soup = BeautifulSoup(html_content, 'html.parser')
    
    # The DeprecationWarning is because the 'text' argument is old. Using 'string' is the correct way.
    # The code below is corrected to reflect the new standard.
    table = soup.find('table', {'class': 'wikitable sortable css-serial'})
    
    if table is None:
        print("Error: Table with class 'wikitable sortable css-serial' not found!")
        return []

    scientific_names = []
    for row in table.find_all('tr')[1:]:
        cells = row.find_all('td')
        if len(cells) > 1:
            italicized = cells[1].find('i')
            if italicized:
                scientific_name = italicized.get_text(strip=True)
                scientific_names.append(scientific_name)
    return scientific_names

def get_wikidata_q_number(species_name: str) -> str:
    """
    Fetches the Wikidata Q-number for a given species from its Wikipedia page.
    """
    wikipedia_url = f'https://en.wikipedia.org/wiki/{species_name.replace(" ", "_")}'
    headers = {'User-Agent': WIKI_USER_AGENT}
    try:
        response = requests.get(wikipedia_url, headers=headers)
        response.raise_for_status()
        soup = BeautifulSoup(response.content, 'html.parser')
        
        # Use 'string' instead of 'text' to resolve the DeprecationWarning
        wikidata_link = soup.find('a', href=True, string='Wikidata item')
        if wikidata_link:
            return wikidata_link['href'].split('/')[-1]
        return None
    except requests.exceptions.RequestException as e:
        print(f"Error fetching Q-number for {species_name}: {e}")
        return None

def fetch_sitelinks(q_number: str) -> Dict[str, Any]:
    """
    Fetches sitelinks for a given Wikidata Q-number from the Wikidata API.
    """
    sitelinks = {lang: None for lang in EU_LANGUAGES}
    if not q_number or pd.isna(q_number):
        return sitelinks

    url = f"https://www.wikidata.org/wiki/Special:EntityData/{q_number}.json"
    headers = {'User-Agent': WIKI_USER_AGENT}
    try:
        response = requests.get(url, headers=headers)
        response.raise_for_status()
        data = response.json()
        entity_data = data.get('entities', {}).get(q_number, {})
        
        if not entity_data:
            print(f"No entity data found for Q-number {q_number}")
            return sitelinks

        sitelinks_data = entity_data.get('sitelinks', {})
        for key, value in sitelinks_data.items():
            lang = key.split('wiki')[0]
            if lang in EU_LANGUAGES:
                sitelinks[lang] = value['title']
        return sitelinks
    except requests.exceptions.RequestException as e:
        print(f"Error fetching sitelinks for Q-number {q_number}: {e}")
        return sitelinks

def main():
    """Main function to orchestrate the data extraction process."""
    url = 'https://en.wikipedia.org/wiki/List_of_invasive_alien_species_of_Union_concern'
    
    print("Step 1: Extracting scientific names...")
    response = fetch_webpage_content(url)
    if not response:
        sys.exit()
    
    scientific_names = extract_scientific_names(response.content)
    if not scientific_names:
        print("No scientific names found. Exiting.")
        sys.exit()

    df = pd.DataFrame({'Scientific Name': scientific_names})
    print(df.head())

    print("\nStep 2: Getting Wikidata Q-numbers...")
    # Use .progress_apply() instead of .apply() to show the progress bar.
    df['Wikidata Q-number'] = df['Scientific Name'].progress_apply(get_wikidata_q_number)
    print(df.head())
    
    print("\nStep 3: Fetching sitelinks for all EU languages...")
    all_sitelinks = []
    
    # Use tqdm to wrap the loop for a progress bar
    for _, row in tqdm(df.iterrows(), total=len(df), desc="Fetching sitelinks"):
        q_number = row['Wikidata Q-number']
        species_name = row['Scientific Name']
        if pd.isna(q_number):
            continue
        sitelinks = fetch_sitelinks(q_number)
        
        for lang, title in sitelinks.items():
            if title:
                all_sitelinks.append({
                    'Scientific Name': species_name,
                    'Q-number': q_number,
                    'Language': lang,
                    'Wikipedia Title': title
                })

    sitelinks_df = pd.DataFrame(all_sitelinks)
    print(sitelinks_df.head())
    
    print("\nStep 4: Saving data to CSV files...")
    df.to_csv('species_q_numbers.csv', index=False)
    sitelinks_df.to_csv('species_wikipedia_sitelinks.csv', index=False)
    print("All data saved successfully.")

if __name__ == '__main__':
    main()