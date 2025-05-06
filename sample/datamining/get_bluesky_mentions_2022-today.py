from atproto import Client
import time
import pandas as pd
from langdetect import detect, DetectorFactory
from tqdm import tqdm
from dotenv import load_dotenv
import os

# Load environment variables
load_dotenv()

# Seed langdetect for reproducibility
DetectorFactory.seed = 0

# Initialize Bluesky client
client = Client()
username = os.getenv("BSKY_USERNAME")
password = os.getenv("BSKY_PASSWORD")
profile = client.login(username, password)
print('Welcome,', profile.display_name)

# --- Load species list ---
species_df = pd.read_csv("list_of_union_concern.csv")
scientific_names = (
    species_df["Scientific Name"]
    .dropna()  # remove blanks
    .unique()  # deduplicate
    .tolist()  # convert to list
)
print(f"Found {len(scientific_names)} unique scientific names.")

# Time window for searching
since_date = "2022-01-01T00:00:00Z"
until_date = "2025-04-15T00:00:00Z"
limit_per_call = 100

# EU and regional languages whitelist (ISO 639-1 codes)
allowed_languages = {
    # Official EU languages (24)
    'bg', 'hr', 'cs', 'da', 'nl', 'et', 'fi', 'fr', 'de', 'el',
    'hu', 'ga', 'it', 'lv', 'lt', 'mt', 'pl', 'pt', 'ro', 'sk',
    'sl', 'es', 'sv',

    # Widely spoken non-official or regional languages in EU
    'en', 'ca', 'eu', 'gl', 'oc', 'sc', 'co', 'frp', 'cy', 'lb',

    # Full coverage of Scandinavian & Nordic region languages
    'no', 'nb', 'nn', 'se', 'is',  # Norwegian (Bokmål & Nynorsk), Sami, Icelandic
}


# Dictionary to store unique posts (keyed by post.uri)
unique_posts = {}

# Iterate over each scientific name
for idx, name in tqdm(enumerate(scientific_names), total=len(scientific_names), desc="Cycling through species", unit="species"):
    print(f"\nSearching posts for species: {name}")
    params = {
        "q": f'"{name}"',  # exact match search
        "limit": limit_per_call,
        "sort": "latest",
        "since": since_date,
        "until": until_date
    }
    cursor = None
    species_count = 0

    while True:
        if cursor:
            params["cursor"] = cursor

        try:
            response = client.app.bsky.feed.search_posts(params)
        except Exception as e:
            print(f"  Error fetching posts for '{name}': {e}")
            break

        if not response.posts:
            print("  No more posts found.")
            break

        print(f"  Fetched {len(response.posts)} posts")
        new_count = 0

        for post in response.posts:
            post_id = post.uri
            if post_id in unique_posts:
                continue

            post_text = post.record.text
            try:
                language = detect(post_text)
            except Exception:
                language = "und"

            location = getattr(post.author, 'location', "Unknown")
            unique_posts[post_id] = {
                "Created_At": post.record.created_at,
                "Text": post_text,
                "Location": location,
                "Language": language
            }
            new_count += 1
            species_count += 1

        if new_count == 0:
            print("  No new unique posts added, moving to next species.")
            break

        cursor = response.cursor
        if not cursor:
            print("  No more pages available, done with this species.")
            break

        time.sleep(2)  # backoff between calls

    # Save interim backup after each species
    print(f"Saving backup after processing: {name} ({species_count} posts found)")
    backup_df = pd.DataFrame(list(unique_posts.values()))
    backup_df.to_csv("bluesky_posts_eu.csv", index=False)

# Filter posts to keep only those with allowed EU languages
filtered_posts = {
    pid: post for pid, post in unique_posts.items()
    if post["Language"] in allowed_languages
}

print(f"\nTotal unique posts collected: {len(unique_posts)}")
print(f"Posts after filtering by allowed EU languages: {len(filtered_posts)}")
    
# Save final CSV
final_df = pd.DataFrame(list(filtered_posts.values()))
final_df.to_csv("bluesky_posts_eu.csv", index=False)
print("Final data saved to bluesky_posts_eu.csv")
