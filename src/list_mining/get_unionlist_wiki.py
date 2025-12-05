"""
EASIN Species Wikipedia Sitelinks Pipeline

Fetches invasive species from Wikipedia, retrieves their Wikidata Q-numbers,
and collects Wikipedia article titles across multiple EU languages.
"""

import os
import sys
import requests
import pandas as pd
from bs4 import BeautifulSoup
from typing import List, Dict, Any, Tuple
from dotenv import load_dotenv
from tqdm.auto import tqdm

# Load environment variables from .env file
load_dotenv()

# --- Configuration Constants ---
# Define the official EU languages (ISO 639-1 codes)
EU_LANGUAGES = [
    "bg", "cs", "da", "de", "el", "en", "es", "et", "fi", "fr", "ga", "hr",
    "hu", "it", "lt", "lv", "mt", "nl", "pl", "pt", "ro", "sk", "sl", "sv",
    "is", "no",  # Non-EU EEA languages often included
    "eu", "gl", "ca", "sq", "bs", "mk", "sr", "tr",  # Other regional/related
    "cy"
]

# Get the user-agent string from an environment variable
WIKI_USER_AGENT = os.getenv("WIKI_USER_AGENT")

# Set a default user-agent if the environment variable is not set
if not WIKI_USER_AGENT:
    print("Warning: WIKI_USER_AGENT environment variable not set. "
          "Using a default user-agent - MIGHT GET BLOCKED.")
    WIKI_USER_AGENT = (
        'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 '
        '(KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3'
    )

# Initialize tqdm for pandas
tqdm.pandas()
# -----------------------------


def fetch_webpage_content(url: str) -> requests.Response:
    """
    Fetch the content of a webpage with a User-Agent header.
    
    Args:
        url: The URL to fetch
        
    Returns:
        Response object or None if request fails
    """
    headers = {'User-Agent': WIKI_USER_AGENT}
    try:
        response = requests.get(url, headers=headers)
        response.raise_for_status()
        return response
    except requests.exceptions.RequestException as e:
        print(f"Error fetching URL {url}: {e}")
        return None


def extract_scientific_names(html_content: bytes) -> List[str]:
    """
    Parse HTML content to find species table and extract scientific names.
    
    Args:
        html_content: Raw HTML content as bytes
        
    Returns:
        List of scientific names
    """
    soup = BeautifulSoup(html_content, 'html.parser')
    
    # Target the known table class for the list of species
    table = soup.find('table', {'class': 'wikitable sortable css-serial'})
    
    if table is None:
        print("Error: Table with class 'wikitable sortable css-serial' "
              "not found!")
        return []

    scientific_names = []
    # Skip the header row (index 0)
    for row in table.find_all('tr')[1:]:
        cells = row.find_all('td')
        if len(cells) > 1:
            # Scientific names are usually in italics in the second column
            italicized = cells[1].find('i')
            if italicized:
                scientific_name = italicized.get_text(strip=True)
                scientific_names.append(scientific_name)
    return scientific_names


def get_wikidata_q_number(species_name: str) -> str:
    """
    Fetch the Wikidata Q-number for a given species from Wikipedia.
    
    This function should be used with pandas.progress_apply.
    
    Args:
        species_name: Scientific name of the species
        
    Returns:
        Wikidata Q-number or None if not found
    """
    wikipedia_url = (
        f'https://en.wikipedia.org/wiki/{species_name.replace(" ", "_")}'
    )
    headers = {'User-Agent': WIKI_USER_AGENT}
    try:
        response = requests.get(wikipedia_url, headers=headers)
        response.raise_for_status()
        soup = BeautifulSoup(response.content, 'html.parser')
        
        # Find the link to the Wikidata item
        wikidata_link = soup.find('a', href=True, string='Wikidata item')
        if wikidata_link:
            return wikidata_link['href'].split('/')[-1]
        return None
    except requests.exceptions.RequestException:
        # Suppress printing the error inside progress_apply for cleaner output
        return None


def fetch_sitelinks(q_number: str) -> Dict[str, Any]:
    """
    Fetch Wikipedia sitelinks for a Wikidata Q-number across EU languages.
    
    Args:
        q_number: Wikidata Q-number
        
    Returns:
        Dictionary mapping language codes to Wikipedia titles
    """
    sitelinks = {lang: None for lang in EU_LANGUAGES}
    if pd.isna(q_number) or not q_number:
        return sitelinks

    url = f"https://www.wikidata.org/wiki/Special:EntityData/{q_number}.json"
    headers = {'User-Agent': WIKI_USER_AGENT}
    try:
        response = requests.get(url, headers=headers)
        response.raise_for_status()
        data = response.json()
        entity_data = data.get('entities', {}).get(q_number, {})
        
        if not entity_data:
            return sitelinks

        sitelinks_data = entity_data.get('sitelinks', {})
        for key, value in sitelinks_data.items():
            lang = key.split('wiki')[0]
            if lang in EU_LANGUAGES:
                sitelinks[lang] = value['title']
        return sitelinks
    except requests.exceptions.RequestException:
        return sitelinks


def run_wiki_sitelinks_pipeline(
    wiki_url: str = 'https://en.wikipedia.org/wiki/'
                    'List_of_invasive_alien_species_of_Union_concern',
    q_number_file: str = 'species_q_numbers.csv',
    sitelinks_file: str = 'species_wikipedia_sitelinks.csv'
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """
    Orchestrate the data pipeline for species sitelinks collection.
    
    Fetches species names from Wikipedia, finds their Wikidata Q-numbers,
    retrieves multi-lingual Wikipedia sitelinks, and saves the data.

    Args:
        wiki_url: Wikipedia URL containing the species list table
        q_number_file: Output filename for scientific names and Q-numbers
        sitelinks_file: Output filename for all sitelinks data

    Returns:
        Tuple of (q_numbers_dataframe, sitelinks_dataframe)
    """
    print(f"--- Starting Pipeline for URL: {wiki_url} ---")
    
    # --- Step 1: Extract scientific names ---
    print("Step 1/4: Fetching webpage and extracting scientific names...")
    response = fetch_webpage_content(wiki_url)
    if not response:
        print("Failed to fetch webpage content. Pipeline exiting.")
        return pd.DataFrame(), pd.DataFrame()
    
    scientific_names = extract_scientific_names(response.content)
    if not scientific_names:
        print("No scientific names found. Pipeline exiting.")
        return pd.DataFrame(), pd.DataFrame()

    df_q_numbers = pd.DataFrame({'Scientific Name': scientific_names})

    # --- Step 2: Get Wikidata Q-numbers ---
    print("Step 2/4: Getting Wikidata Q-numbers (This may take time)...")
    df_q_numbers['Wikidata Q-number'] = (
        df_q_numbers['Scientific Name'].progress_map(get_wikidata_q_number)
    )
    
    # --- Step 3: Fetch sitelinks ---
    print("Step 3/4: Fetching sitelinks for all EU languages "
          "(This may take time)...")
    all_sitelinks = []
    
    # Filter out rows where Q-number is missing before iterating
    df_q_numbers_valid = df_q_numbers.dropna(subset=['Wikidata Q-number'])

    for _, row in tqdm(df_q_numbers_valid.iterrows(),
                       total=len(df_q_numbers_valid),
                       desc="Fetching sitelinks"):
        q_number = row['Wikidata Q-number']
        species_name = row['Scientific Name']
        
        sitelinks = fetch_sitelinks(q_number)
        
        for lang, title in sitelinks.items():
            if title:
                all_sitelinks.append({
                    'Scientific Name': species_name,
                    'Q-number': q_number,
                    'Language': lang,
                    'Wikipedia Title': title
                })

    df_sitelinks = pd.DataFrame(all_sitelinks)
    
    # --- Step 4: Save data ---
    print(f"Step 4/4: Saving data to {q_number_file} and {sitelinks_file}...")
    df_q_numbers.to_csv(q_number_file, index=False)
    df_sitelinks.to_csv(sitelinks_file, index=False)
    
    print("Pipeline completed and data saved successfully.")
    
    return df_q_numbers, df_sitelinks


def main():
    """Main execution function."""
    # Define variables here for easy testing/modification
    wiki_url = (
        'https://en.wikipedia.org/wiki/'
        'List_of_invasive_alien_species_of_Union_concern'
    )
    q_number_output = 'species_q_numbers.csv'
    sitelinks_output = 'species_wikipedia_sitelinks.csv'
    
    # Run the pipeline
    q_df, links_df = run_wiki_sitelinks_pipeline(
        wiki_url,
        q_number_output,
        sitelinks_output
    )
    
    print("\n--- Final DataFrames Head ---")
    print("Q-Numbers:")
    print(q_df.head())
    print("\nSitelinks:")
    print(links_df.head())


if __name__ == "__main__":
    main()