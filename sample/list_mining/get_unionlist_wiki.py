#import dependencies
import sys
import pandas as pd
import requests
from bs4 import BeautifulSoup

# URL of the Wikipedia page
url = 'https://en.wikipedia.org/wiki/List_of_invasive_alien_species_of_Union_concern'

# Fetch the webpage content
response = requests.get(url)
soup = BeautifulSoup(response.content, 'html.parser')

# Debugging: Print the first 500 characters of the page
print(soup.prettify()[:500])

# Find the specific table by class name
table = soup.find('table', {'class': 'wikitable sortable css-serial'})

# Check if the table is found
if table is None:
    print("Error: Table with class 'wikitable sortable' not found!")
    sys.exit()

# Extract scientific names
scientific_names = []

for row in table.find_all('tr')[1:]:  # Skip the header row
    cells = row.find_all('td')
    if len(cells) > 1:  # Ensure there are enough columns
        # Find italicized text in the "Scientific name" column
        italicized = cells[1].find('i')
        if italicized:
            scientific_name = italicized.get_text(strip=True)
            scientific_names.append(scientific_name)

# Create a DataFrame
df = pd.DataFrame({'Scientific Name': scientific_names})

# Display the DataFrame
print(df)

# Save to a CSV file
output_filename = 'list_of_union_concern.csv'
df.to_csv(output_filename)

# Function to get Wikidata Q-number from a Wikipedia page
def get_q_number(species_name):
    wikipedia_url = f'https://en.wikipedia.org/wiki/{species_name.replace(" ", "_")}'
    try:
        response = requests.get(wikipedia_url)
        soup = BeautifulSoup(response.content, 'html.parser')
        # Find the link to Wikidata (usually near the bottom of the page)
        wikidata_link = soup.find('a', href=True, text='Wikidata item')
        if wikidata_link:
            # Extract the Q-number from the URL
            q_number = wikidata_link['href'].split('/')[-1]
            return q_number
        else:
            return None
    except Exception as e:
        print(f"Error fetching Q-number for {species_name}: {e}")
        return None

# Extract Q-numbers for all species
df['Wikidata Q-number'] = df['Scientific Name'].apply(get_q_number)

# Save the resulting DataFrame to a CSV file
df.to_csv('species_q_numbers.csv', index=False)

# Define the official EU languages (ISO 639-1 codes)
eu_languages = [
    "bg", "cs", "da", "de", "el", "en", "es", "et", "fi", "fr", "ga", "hr",
    "hu", "it", "lt", "lv", "mt", "nl", "pl", "pt", "ro", "sk", "sl", "sv", 
    "is", "no",  # Icelandic and Norwegian (official languages of Norway)
    "eu", "gl", "ca", "sq", "bs", "mk", "sr", "tr",  # Regional and minority languages
    "cy"  # Greek Cypriot dialect (Cyprus)
]

# Load the species Q-numbers from the previously saved CSV file
df = pd.read_csv('species_q_numbers.csv')

# Function to fetch sitelinks from Wikidata for each Q-number
def fetch_sitelinks(q_number):
    sitelinks = {lang: None for lang in eu_languages}  # Initialize sitelinks for each language
    if pd.isna(q_number):  # Skip if no Q-number
        return sitelinks
    url = f"https://www.wikidata.org/wiki/Special:EntityData/{q_number}.json"
    try:
        response = requests.get(url)
        data = response.json()
        entity_data = data.get('entities', {}).get(q_number, {})
        if not entity_data:
            print(f"No entity data found for Q-number {q_number}")
            return sitelinks
        # Extract sitelinks from the JSON response
        sitelinks_data = entity_data.get('sitelinks', {})
        for key, value in sitelinks_data.items():
            lang = key.split('wiki')[0]  # Extract the language code (e.g., 'en', 'de')
            if lang in eu_languages:  # Filter for EU languages
                sitelinks[lang] = value['title']
    except Exception as e:
        print(f"Error fetching sitelinks for Q-number {q_number}: {e}")
    return sitelinks

# Extract sitelinks for all species
all_sitelinks = []

for _, row in df.iterrows():
    q_number = row['Wikidata Q-number']
    species_name = row['Scientific Name']
    if pd.isna(q_number):  # Skip if no Q-number
        continue
    sitelinks = fetch_sitelinks(q_number)
    for lang, title in sitelinks.items():
        if title:  # Only add if the sitelink exists
            all_sitelinks.append({
                'Scientific Name': species_name,
                'Q-number': q_number,
                'Language': lang,
                'Wikipedia Title': title
            })

# Create a DataFrame of all sitelinks
sitelinks_df = pd.DataFrame(all_sitelinks)

# Save to CSV
sitelinks_df.to_csv('species_wikipedia_sitelinks.csv', index=False)

# Display a sample of the DataFrame
print(sitelinks_df.head())
