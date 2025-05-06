import praw
import pandas as pd
from datetime import datetime, timezone
import time
import os
from dotenv import load_dotenv
from langdetect import detect, DetectorFactory, LangDetectException
import re

# Ensure deterministic results from langdetect
DetectorFactory.seed = 0

# Initialize Reddit client
load_dotenv()
reddit = praw.Reddit(
    client_id=os.getenv("REDDIT_ID"),
    client_secret=os.getenv("REDDIT_SECRET"),
    user_agent=os.getenv("REDDIT_AGENT")
)

# --- Load species list ---
species_df = pd.read_csv("list_of_union_concern.csv")
species_list = (
    species_df["Scientific Name"]
    .dropna()
    .unique()
    .tolist()
)
print(f"Found {len(species_list)} unique scientific names.")

# Set up Reddit API
all_subreddits = reddit.subreddit("all")

# Prepare DataFrame containers
titles = []
scores = []
ids = []
post_times = []
urls = []
selftexts = []
subreddits = []
user_locations = []
mentioned_species = []
species_searched = []
languages = []

# Define time window: Jan 1, 2022 → now
start_time = datetime(2022, 1, 1, tzinfo=timezone.utc)
end_time   = datetime.now(timezone.utc)
start_ts   = int(start_time.timestamp())
end_ts     = int(end_time.timestamp())

print(f"Collecting posts from {start_time.date()} to {end_time.date()}.")

total_species   = len(species_list)
species_counter = 0

for species in species_list:
    species_counter += 1
    species_start_time = time.time()

    # Wrap species in quotes for exact-match search
    query   = f'"{species}"'
    pattern = re.compile(rf'\b{re.escape(species)}\b', re.IGNORECASE)

    print(f"\n=== [{species_counter}/{total_species}] Searching for: {query} ===")
    post_counter = 0

    for submission in all_subreddits.search(
        query,
        limit=1000,
        syntax="cloudsearch",
        time_filter="all",
    ):
        # parse timestamp once
        submission_time = datetime.fromtimestamp(
            submission.created_utc,
            tz=timezone.utc
        )

        # skip if outside desired window
        if not (start_ts <= submission.created_utc <= end_ts):
            continue

        # combine text and verify exact-phrase match
        text = submission.title + " " + submission.selftext
        if not pattern.search(text):
            continue

        #— at this point it's a keeper —
        post_counter += 1
        titles.append(submission.title)
        scores.append(submission.score)
        ids.append(submission.id)
        post_times.append(submission_time)
        urls.append(submission.url)
        selftexts.append(submission.selftext)
        subreddits.append(submission.subreddit.display_name)

        # user flair text
        user_locations.append(
            submission.author_flair_text or "Not Available"
        )

        # you know it matches exactly
        mentioned_species.append(species)
        species_searched.append(species)

        # language detection
        try:
            languages.append(detect(text))
        except LangDetectException:
            languages.append("Unknown")

        # rate-limit safeguard
        time.sleep(1)
        print(f"  → Post {post_counter}: [{submission.id}] | ✓")

    # report species-level timing
    elapsed = time.time() - species_start_time
    print(f"✔ Finished {species} — {post_counter} posts; elapsed {elapsed:.1f}s")
    print(f"Progress: {species_counter/total_species*100:.1f}%")

# build DataFrame
df = pd.DataFrame({
    "Title":             titles,
    "Id":                ids,
    "Upvotes":           scores,
    "Post Time (UTC)":   post_times,
    "URL":               urls,
    "Selftext":          selftexts,
    "Subreddit":         subreddits,
    "User Location":     user_locations,
    "Species Mentioned": mentioned_species,
    "Species Searched":  species_searched,
    "Detected Language": languages
})

print("\n=== Mining complete! ===")
print(df.shape)
print(df.head(10))

# save CSV
df.to_csv(
    "reddit_posts_with_species_language_exact.csv",
    index=False
)
print("✅ Data saved to 'reddit_posts_with_species_language_exact.csv'.")
