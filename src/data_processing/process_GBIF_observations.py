# ==============================================================================
# Script Name: process_GBIF_observations.py
# Description: Processes GBIF species occurrence data by cleaning event dates
#              and pivoting into a time-series dataset (species-country x date).
# Author: Simon Reynaert
# ==============================================================================

import os
from datetime import timedelta
from dateutil import parser
import pandas as pd
from tqdm import tqdm


def parse_event_date(date_string):
    """
    Parse a date string, handling both single dates and short date ranges.

    Parameters
    ----------
    date_string : str
        The date string to parse.

    Returns
    -------
    pd.Timestamp or pd.NaT
        Parsed, timezone-naive datetime, or NaT if parsing fails.
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

            # Make both dates timezone-naive
            if start_date.tzinfo is not None:
                start_date = start_date.astimezone(tz=None).replace(tzinfo=None)
            if end_date.tzinfo is not None:
                end_date = end_date.astimezone(tz=None).replace(tzinfo=None)

            # Accept only short ranges (<= 1 day)
            if (end_date - start_date) <= timedelta(hours=24):
                return start_date
            return pd.NaT
        except Exception:
            return pd.NaT

    # Handle single dates
    try:
        dt = parser.parse(date_string)
        if dt.tzinfo is not None:
            dt = dt.astimezone(tz=None).replace(tzinfo=None)
        return dt
    except Exception:
        return pd.NaT


def process_gbif_data(input_file, output_file=None, failed_output_file=None,
                      start_date=None, end_date=None):
    """
    Process and pivot GBIF observation data into a time-series dataset.

    Parameters
    ----------
    input_file : str
        Path to the input GBIF CSV file.
    output_file : str, optional
        Path for saving the processed pivot CSV.
        If None, automatically saved as '<input_file>_processed.csv'.
    failed_output_file : str, optional
        Path for saving failed date parses.
        If None, automatically saved as '<input_file>_failed_dates.csv'.
    start_date : str or pd.Timestamp, optional
        Earliest date to include in the pivot table.
        If None, inferred from data.
    end_date : str or pd.Timestamp, optional
        Latest date to include in the pivot table.
        If None, inferred from data.

    Returns
    -------
    pd.DataFrame
        Pivoted DataFrame with species-country rows and daily observation counts.

    Examples
    --------
    >>> df = process_gbif_data(
    ...     input_file="GBIF_species_occurrences_EU.csv",
    ...     output_file="GBIF_Observations_2016_present.csv",
    ...     start_date="2016-01-01"
    ... )
    Processed dataset saved to: GBIF_Observations_2016_present.csv
    """
    print(f"Loading data from {input_file}...")
    try:
        df = pd.read_csv(input_file, low_memory=False)
    except FileNotFoundError:
        print(f"Error: The file '{input_file}' was not found.")
        return pd.DataFrame()

    total_rows = len(df)
    print(f"Successfully loaded {total_rows:,} rows.")

    # Ensure output filenames
    if output_file is None:
        base, ext = os.path.splitext(input_file)
        output_file = f"{base}_processed{ext}"
    if failed_output_file is None:
        base, ext = os.path.splitext(input_file)
        failed_output_file = f"{base}_failed_dates{ext}"

    df["raw_eventDate"] = df["eventDate"]

    print("\nParsing event dates...")
    tqdm.pandas(desc="Parsing event dates")
    df["parsed_eventDate"] = df["raw_eventDate"].progress_apply(parse_event_date)

    success_count = df["parsed_eventDate"].notna().sum()
    fail_count = df["parsed_eventDate"].isna().sum()

    print(f"\n--- Parsing Summary ---")
    print(f"Total rows: {total_rows:,}")
    print(f"Parsed successfully: {success_count:,} ({(success_count / total_rows) * 100:.2f}%)")
    print(f"Failed parses: {fail_count:,} ({(fail_count / total_rows) * 100:.2f}%)")

    # Save failed date ranges for inspection
    failed_ranges = df[
        df["raw_eventDate"].str.contains("/", na=False)
        & df["parsed_eventDate"].isna()
    ]
    failed_ranges.to_csv(failed_output_file, index=False)
    print(f"Saved failed date ranges to: {failed_output_file}")

    # Keep only valid parsed dates
    df_valid = df[df["parsed_eventDate"].notna()].copy()
    df_valid["parsed_eventDate"] = pd.to_datetime(df_valid["parsed_eventDate"])
    df_valid["date_str"] = df_valid["parsed_eventDate"].dt.strftime("%Y-%m-%d")

    # Infer start and end dates if not provided
    start_date = pd.to_datetime(start_date) if start_date else df_valid["parsed_eventDate"].min()
    end_date = pd.to_datetime(end_date) if end_date else df_valid["parsed_eventDate"].max()

    print(f"\nCreating time series from {start_date.date()} to {end_date.date()}...")

    # Aggregate and pivot
    grouped = (
        df_valid.groupby(["species", "country", "date_str"])
        .size()
        .reset_index(name="count")
    )

    pivot = grouped.pivot_table(
        index=["species", "country"],
        columns="date_str",
        values="count",
        fill_value=0,
    ).reset_index()

    # Ensure all dates are represented
    all_dates = pd.date_range(start=start_date, end=end_date).strftime("%Y-%m-%d").tolist()
    for date in all_dates:
        if date not in pivot.columns:
            pivot[date] = 0

    # Sort and rename columns
    static_cols = ["species", "country"]
    date_cols = sorted([c for c in pivot.columns if c not in static_cols])
    pivot = pivot[static_cols + date_cols]
    pivot.rename(columns={"species": "Scientific Name", "country": "Country"}, inplace=True)

    pivot.to_csv(output_file, index=False)
    print(f"\nProcessed dataset saved to: {output_file}")

    return pivot


def main():
    """
    Example entry point for manual testing (optional when importing in notebooks).
    """
    input_file = "GBIF_species_occurrences_EU.csv"
    output_file = "GBIF_Observations_2016_present.csv"
    failed_output_file = "failed_parse_dates.csv"
    start_date = "2016-01-01"

    process_gbif_data(
        input_file=input_file,
        output_file=output_file,
        failed_output_file=failed_output_file,
        start_date=start_date,
    )


if __name__ == "__main__":
    main()
