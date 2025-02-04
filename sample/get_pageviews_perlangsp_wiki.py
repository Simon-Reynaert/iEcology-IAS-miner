import subprocess
import sys
import pandas as pd
import requests
from datetime import datetime, timedelta
import time
import numpy as np

# Ensure required packages are installed and up to date
def install_and_update_packages(packages):
    for package in packages:
        subprocess.check_call([sys.executable, "-m", "pip", "install", "--upgrade", package])

required_packages = ['pandas', 'requests', 'numpy']
install_and_update_packages(required_packages)

# Load the Wikipedia sitelinks DataFrame
sitelinks_df = pd.read_csv('species_wikipedia_sitelinks.csv')

# Wikimedia API base URL for per-article pageviews
BASE_URL = "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article"

# Custom User-Agent string
HEADERS = {
    "User-Agent": "iEcology_OneSTOP_EUProject/1.0 (simon.reynaert@plantentuinmeise.be)"
}

# Calculate the previous month for API queries
today = datetime.utcnow().date()
first_day_of_this_month = today.replace(day=1)
last_day_of_last_month = first_day_of_this_month - timedelta(days=1)
year = last_day_of_last_month.year
month = last_day_of_last_month.month

# Calculate date ranges
start_date = last_day_of_last_month.replace(day=1).strftime("%Y%m%d")  # First day of last month
end_date = last_day_of_last_month.strftime("%Y%m%d")  # Last day of last month

# Function to fetch daily user pageviews for a specific article
def fetch_daily_pageviews(language, title):
    project = f"{language}.wikipedia"
    url = f"{BASE_URL}/{project}/all-access/user/{title}/daily/{start_date}/{end_date}"

    try:
        response = requests.get(url, headers=HEADERS)
        response.raise_for_status()
        data = response.json()
        daily_views = {item["timestamp"][:8]: item["views"] for item in data["items"]}  # Extract daily views
    except Exception as e:
        print(f"Error fetching pageviews for {language}:{title} - {e}")
        return {}
    
    return daily_views

# Fetch pageviews and compute statistics
species_pageviews = []
date_columns = [(last_day_of_last_month - timedelta(days=i)).strftime("%Y%m%d") for i in range(last_day_of_last_month.day)][::-1]  # Ensure chronological order

for _, row in sitelinks_df.iterrows():
    language = row['Language']
    title = row['Wikipedia Title'].replace(' ', '_')  # Ensure URL-safe titles
    scientific_name = row['Scientific Name']
    print(f"Fetching data for {scientific_name} ({language}) for {year}-{month:02d}...")

    # Fetch daily user pageviews
    daily_pageviews = fetch_daily_pageviews(language, title)
    if not daily_pageviews:
        species_pageviews.append({
            "Scientific Name": scientific_name,
            "Language": language,
            "Wikipedia Title": title,
            "Mean First 3 Weeks": None,
            "Standard Error First 3 Weeks": None,
            "Mean Last Week": None,
            "Status": "No data"
        })
        for date in date_columns:
            species_pageviews[-1][date] = None
        continue

    # Add daily pageviews for each date in order
    pageviews_list = [daily_pageviews.get(date, 0) for date in date_columns]
    
    # Store daily views in the species_pageviews
    species_pageviews.append({
        "Scientific Name": scientific_name,
        "Language": language,
        "Wikipedia Title": title,
        "Mean First 3 Weeks": None,
        "Standard Error First 3 Weeks": None,
        "Mean Last Week": None,
        "Status": None,
    })

    # Add the pageviews for each day to the row
    for i, date in enumerate(date_columns):
        species_pageviews[-1][date] = pageviews_list[i]

    # Split into time periods for analysis
    first_three_weeks = pageviews_list[:21]
    last_week = pageviews_list[21:]

    # Calculate statistics
    mean_3w = np.mean(first_three_weeks) if first_three_weeks else 0
    se_3w = np.std(first_three_weeks, ddof=1) / np.sqrt(len(first_three_weeks)) if first_three_weeks else 0
    mean_last_week = np.mean(last_week) if last_week else 0

    # Determine if pageviews are increasing or decreasing
    if mean_last_week > (mean_3w + se_3w):
        status = "Increasing"
    elif mean_last_week < (mean_3w - se_3w):
        status = "Decreasing"
    else:
        status = "No change"

    # Update the row with the statistics
    species_pageviews[-1]["Mean First 3 Weeks"] = round(mean_3w, 2)
    species_pageviews[-1]["Standard Error First 3 Weeks"] = round(se_3w, 2)
    species_pageviews[-1]["Mean Last Week"] = round(mean_last_week, 2)
    species_pageviews[-1]["Status"] = status

    # Add delay to avoid rate limiting
    time.sleep(1)

# Create a DataFrame with the aggregated data
species_pageviews_df = pd.DataFrame(species_pageviews)

# Save to CSV
output_file = f'species_pageviews_analysis_{year}_{month:02d}.csv'
species_pageviews_df.to_csv(output_file, index=False)

# Display a sample of the DataFrame
print(f"Pageview Analysis for {year}-{month:02d}:")
print(species_pageviews_df.head())
