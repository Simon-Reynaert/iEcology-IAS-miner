import requests
import pandas as pd
import re
import time
from datetime import datetime, timezone
from bs4 import BeautifulSoup
from tqdm import tqdm
from dotenv import load_dotenv
import os

# --- Load environment variables ---
load_dotenv()
ACCESS_TOKEN = os.getenv("MASTODON_ACCESS_TOKEN")

# --- Date Range ---
START_DATE = datetime(2022, 1, 1, 0, 0, 0, tzinfo=timezone.utc)
END_DATE = datetime.now(timezone.utc)

# --- Test Mode ---
TEST_MODE = False  # Set to True for testing a few species
TEST_LIMIT = 5    # Number of species to process in test mode

# --- Function to clean HTML/text ---
def clean_text(text):
    soup = BeautifulSoup(text, 'html.parser')
    clean = soup.get_text()
    clean = re.sub(r'[^\x00-\x7F]+', '', clean)  # Remove non-ASCII chars
    return clean.strip()

# --- Load species list ---
try:
    species_df = pd.read_csv("list_of_union_concern.csv")
except FileNotFoundError:
    print("Error: CSV file not found.")
    exit()
except pd.errors.EmptyDataError:
    print("Error: CSV file is empty.")
    exit()
except pd.errors.ParserError:
    print("Error: CSV file parsing error.")
    exit()

# Extract unique scientific names
species_list = species_df["Scientific Name"].dropna().unique().tolist()
if TEST_MODE:
    species_list = species_list[:TEST_LIMIT]

# --- Mastodon search setup ---
search_url = 'https://mastodon.social/api/v2/search'
results = []
processed_status_ids = set()

print("Fetching toots…")

for species in tqdm(species_list, total=len(species_list)):
    # Compile regex for full scientific name, case-insensitive
    pattern = re.compile(rf"\b{re.escape(species)}\b", re.IGNORECASE)
    max_id = None
    has_more = True

    while has_more:
        params = {
            'q': species,
            'type': 'statuses',
            'limit': 40,
            'resolve': 'true',
            'max_id': max_id
        }
        headers = {'Authorization': f'Bearer {ACCESS_TOKEN}'}

        try:
            resp = requests.get(search_url, params=params, headers=headers)
            resp.raise_for_status()
            data = resp.json()
            statuses = data.get('statuses', [])
        except Exception as e:
            print(f"Request failed for species '{species}', max_id={max_id}: {e}")
            break

        if not statuses:
            break

        for status in statuses:
            sid = status.get('id')
            if sid in processed_status_ids:
                continue

            # Parse and filter timestamp
            ts_str = status.get('created_at')
            try:
                ts = datetime.fromisoformat(ts_str.replace('Z', '+00:00'))
            except Exception:
                continue
            if not START_DATE <= ts <= END_DATE:
                continue

            content = clean_text(status.get('content', ''))
            if not pattern.search(content):
                continue

            acct = status.get('account', {})
            location = acct.get('location') or clean_text(acct.get('note', ''))

            results.append({
                'Scientific Name': species,
                'Search Keyword': species,
                'Timestamp': ts.isoformat(),
                'Content': content,
                'Language': status.get('language'),
                'Username': acct.get('username'),
                'Location': location or ''
            })
            processed_status_ids.add(sid)

        # Handle pagination via Link header
        link = resp.headers.get('Link', '')
        if 'rel="next"' in link:
            m = re.search(r'max_id=(\d+)', link)
            max_id = m.group(1) if m else None
        else:
            has_more = False

        time.sleep(1)

# --- Save results ---
try:
    df = pd.DataFrame(results)
    if not df.empty:
        df.to_csv('mastodon_species_search_results.csv', index=False)
        print(f"Saved {len(df)} results to mastodon_species_search_results.csv")
    else:
        print("No toots found matching the criteria.")
except Exception as e:
    print(f"Error saving to CSV: {e}")
    if results:
        try:
            df = pd.DataFrame(results)
            df.to_csv('mastodon_species_search_results_partial.csv', index=False)
            print("Partial results saved.")
        except Exception as pe:
            print(f"Failed to save partial results: {pe}")
