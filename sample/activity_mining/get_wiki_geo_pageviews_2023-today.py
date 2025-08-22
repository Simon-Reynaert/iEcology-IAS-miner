#load dependencies
import os
import time
import random
import requests
import pandas as pd
from datetime import datetime, timedelta
from io import StringIO
from dotenv import load_dotenv

# 0. Load environment variables from a .env file for user agent identification
load_dotenv()
USER_AGENT = os.getenv("WIKI_USER_AGENT")

# 1. Read the species_q_numbers.csv file and build a mapping.
# Expected columns: "Scientific Name","Wikidata Q-number"
species_df = pd.read_csv("species_q_numbers.csv")
q_to_name = dict(zip(species_df["Wikidata Q-number"].astype(str), species_df["Scientific Name"]))

# 2. Define the date range: from start_date (limited by data availability in endpoint) to custom defined amount of days.
start_date = datetime.strptime("2023-02-06", "%Y-%m-%d")

end_date = datetime.today()
today = datetime.today()

#To test use code below
#end_date = start_date + timedelta(days=5)  # First 15 days (inclusive)
#all_dates = [(start_date + timedelta(days=i)).strftime("%Y-%m-%d") for i in range(5)]

all_dates = [(start_date + timedelta(days=i)).strftime("%Y-%m-%d") for i in range((today - start_date).days + 1)]

# 3. Set the output filename and load existing data if available.
output_filename = "species_pageviews_wiki_geolocated_2023-02-06_now.csv"
if os.path.exists(output_filename):
    wide_df = pd.read_csv(output_filename)
    # Get existing date columns (skip "Scientific Name", "Country", "Wikidata Q-number")
    existing_date_cols = [col for col in wide_df.columns if col not in ["Scientific Name", "Country", "Wikidata Q-number"]]
else:
    wide_df = pd.DataFrame(columns=["Scientific Name", "Country", "Wikidata Q-number"])
    existing_date_cols = []

# 4. Determine missing dates (those not already present as columns)
missing_dates = [d for d in all_dates if d not in existing_date_cols]
print(f"Querying with missing dates: {missing_dates}")

# 5. Wikimedia URL and headers (i.e. custom user agent string for identification)
base_url = "https://analytics.wikimedia.org/published/datasets/country_project_page"
headers = {"User-Agent": USER_AGENT}

# 6. Process each missing date.
for date_str in missing_dates:
    file_url = f"{base_url}/{date_str}.tsv"
    print(f"Fetching data for {date_str} ...")
    
    try:
        response = requests.get(file_url, headers=headers)
        if response.status_code != 200:
            print(f"Warning: Data for {date_str} not found (status {response.status_code}). Skipping this date.")
            continue  # Skip adding a column if the data retrieval fails.
        # Read TSV data (assumed columns):
        # 0: Country, 1: Country Code, 2: Project, 3: Wiki Page ID, 4: Article Title,
        # 5: Wikidata Q-number, 6: Pageviews
        day_df = pd.read_csv(StringIO(response.text), sep="\t", header=None,
                             names=["Country", "Country Code", "Project", "Wiki Page ID", "Article Title", "Wikidata Q-number", "Pageviews"])
    except Exception as e:
        print(f"Error fetching data for {date_str}: {e}. Skipping this date.")
        continue  # Skip this date if an exception occurs.
    
    # Filter for rows with a Wikidata Q-number in our mapping.
    if not day_df.empty:
        day_df["Wikidata Q-number"] = day_df["Wikidata Q-number"].astype(str)
        day_filtered = day_df[day_df["Wikidata Q-number"].isin(q_to_name.keys())].copy()
        day_filtered["Pageviews"] = pd.to_numeric(day_filtered["Pageviews"], errors="coerce")
        # Group by Country and Wikidata Q-number and sum pageviews.
        day_grouped = day_filtered.groupby(["Country", "Wikidata Q-number"])["Pageviews"].sum().reset_index()
    else:
        day_grouped = pd.DataFrame(columns=["Country", "Wikidata Q-number", "Pageviews"])
    
    # Build a DataFrame for this date with columns: "Scientific Name", "Country", "Wikidata Q-number", and date_str.
    if not day_grouped.empty:
        day_grouped["Scientific Name"] = day_grouped["Wikidata Q-number"].map(q_to_name)
        day_data = day_grouped[["Scientific Name", "Country", "Wikidata Q-number", "Pageviews"]].copy()
        day_data = day_data.rename(columns={"Pageviews": date_str})
    else:
        day_data = pd.DataFrame(columns=["Scientific Name", "Country", "Wikidata Q-number", date_str])
    
    # Merge the day's data with the existing wide DataFrame using an outer join on key columns.
    if wide_df.empty:
        wide_df = day_data.copy()
    else:
        wide_df = pd.merge(wide_df, day_data, on=["Scientific Name", "Country", "Wikidata Q-number"], how="outer")
    
    # Order the columns: "Scientific Name", "Country", "Wikidata Q-number", then date columns in chronological order.
    non_date_cols = ["Scientific Name", "Country", "Wikidata Q-number"]
    date_cols = sorted([col for col in wide_df.columns if col not in non_date_cols])
    wide_df = wide_df[non_date_cols + date_cols]
    
    # Save the updated DataFrame to CSV after processing this date.
    wide_df.to_csv(output_filename, index=False)
    print(f"Data for {date_str} processed and saved to {output_filename}.")
    
    # Sleep for 1-3 second to be polite.
    time.sleep(random.uniform(1, 3))


print(f"All available data processed. Final data saved to {output_filename}.")