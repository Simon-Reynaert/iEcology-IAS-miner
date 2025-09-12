# ==============================================================================
# Script Name: process_GBIF_observations.py
# Description: This script processes a large CSV file of species occurrences,
#              parses and cleans the event dates, and then pivots the data
#              to a wide format. The final output is a CSV file with one
#              row per species-country combination and columns representing
#              daily occurrence counts.
# Author: Simon Reynaert
# ==============================================================================

# --- Load Dependencies ---
import pandas as pd
from dateutil import parser
from datetime import timedelta
from tqdm import tqdm

# --- Core Functions ---
def parse_event_date(date_string):
    """
    Parses a date string, handling single dates and date ranges.

    Args:
        date_string (str): The date string to parse.

    Returns:
        pd.Timestamp or pd.NaT: The parsed, timezone-naive datetime, or
                                 pd.NaT if parsing fails.
    """
    if pd.isna(date_string):
        return pd.NaT

    date_string = str(date_string).strip()

    # Handle date ranges (e.g., "2023-01-01/2023-01-02")
    if "/" in date_string:
        parts = date_string.split("/")
        if len(parts) != 2:
            return pd.NaT
        try:
            start_date = parser.parse(parts[0].strip())
            end_date = parser.parse(parts[1].strip())

            if start_date.tzinfo is not None:
                start_date = start_date.astimezone(tz=None).replace(tzinfo=None)
            if end_date.tzinfo is not None:
                end_date = end_date.astimezone(tz=None).replace(tzinfo=None)

            if (end_date - start_date) <= timedelta(hours=24):
                return start_date
            else:
                return pd.NaT
        except Exception:
            return pd.NaT
    else:
        try:
            dt = parser.parse(date_string)
            if dt.tzinfo is not None:
                dt = dt.astimezone(tz=None).replace(tzinfo=None)
            return dt
        except Exception:
            return pd.NaT

def process_gbif_data(input_file, output_file, failed_output_file):
    """
    Orchestrates the entire data processing pipeline.

    Args:
        input_file (str): Path to the input CSV file.
        output_file (str): Path to the final output CSV file.
        failed_output_file (str): Path to the CSV for failed parses.
    """
    print(f"Loading data from {input_file}...")
    try:
        df = pd.read_csv(input_file, low_memory=False)
        total_rows = len(df)
        print(f"Successfully loaded {total_rows} rows.")
    except FileNotFoundError:
        print(f"Error: The file '{input_file}' was not found.")
        return

    df['raw_eventDate'] = df['eventDate']

    print(f"\nStarting to parse event dates for {total_rows} entries...")

    tqdm.pandas(desc="Parsing event dates")
    df['parsed_eventDate'] = df['raw_eventDate'].progress_apply(parse_event_date)

    print("\nDate parsing complete.")

    successfully_parsed = df['parsed_eventDate'].notna().sum()
    failed_parses = df['parsed_eventDate'].isna().sum()

    percentage_successful = (successfully_parsed / total_rows) * 100
    percentage_failed = (failed_parses / total_rows) * 100

    print(f"\n--- Parsing Summary ---")
    print(f"Total entries processed: {total_rows}")
    print(f"Successfully parsed dates: {successfully_parsed} ({percentage_successful:.2f}%)")
    print(f"Failed to parse dates: {failed_parses} ({percentage_failed:.2f}%)")
    print(f"-----------------------\n")

    failed_date_ranges = df[df['raw_eventDate'].str.contains("/", na=False) & df['parsed_eventDate'].isna()]
    print(f"Number of failed date range parses specifically: {len(failed_date_ranges)}")
    failed_date_ranges.to_csv(failed_output_file, index=False)
    print(f"Saved failed date range parses to {failed_output_file}")

    df_valid = df[df['parsed_eventDate'].notna()].copy()

    df_valid['parsed_eventDate'] = pd.to_datetime(df_valid['parsed_eventDate'])
    df_valid['date_str'] = df_valid['parsed_eventDate'].dt.strftime('%Y-%m-%d')

    print("Grouping and pivoting data...")

    grouped = df_valid.groupby(['species', 'country', 'date_str']).size().reset_index(name='count')

    pivot = grouped.pivot_table(
        index=['species', 'country'],
        columns='date_str',
        values='count',
        fill_value=0
    ).reset_index()

    start_date = pd.to_datetime('2016-01-01')
    end_date = df_valid['parsed_eventDate'].max()
    all_dates = pd.date_range(start=start_date, end=end_date).strftime('%Y-%m-%d').tolist()

    for date in all_dates:
        if date not in pivot.columns:
            pivot[date] = 0

    static_cols = ['species', 'country']
    date_cols = sorted([col for col in pivot.columns if col not in static_cols])
    pivot = pivot[static_cols + date_cols]

    pivot.rename(columns={'species': 'Scientific Name', 'country': 'Country'}, inplace=True)

    print(f"\nSaving final pivot output to {output_file}...")
    pivot.to_csv(output_file, index=False)
    print("Process complete.")

# --- Main Script Execution ---
if __name__ == "__main__":
    INPUT_FILE = "GBIF_species_occurrences_EU.csv"
    OUTPUT_FILE = "GBIF_Observations_2016-present_final_test.csv"
    FAILED_OUTPUT_FILE = "failed_parse_dates_2016-now_test.csv"
    
    process_gbif_data(INPUT_FILE, OUTPUT_FILE, FAILED_OUTPUT_FILE)
