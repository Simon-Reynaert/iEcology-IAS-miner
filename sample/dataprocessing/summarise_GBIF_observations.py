import pandas as pd
from dateutil import parser
from datetime import timedelta

def parse_event_date(x):
    """
    Parse a date string using dateutil.
    
    For strings with a date range (contains "/"):
      - Split into two parts.
      - Parse both dates.
      - If the difference between the two dates is within 24 hours,
        return the first date (converted to tz-naive).
      - Otherwise, return pd.NaT.
        
    For non-range strings, attempt to parse the string.
    If the resulting datetime is tz-aware, convert it to tz-naive.
    """
    if pd.isna(x):
        return pd.NaT

    x = str(x).strip()

    # Handle date ranges
    if "/" in x:
        parts = x.split("/")
        if len(parts) != 2:
            return pd.NaT
        try:
            d1 = parser.parse(parts[0].strip())
            d2 = parser.parse(parts[1].strip())
            # Ensure dates are tz-naive if needed.
            if d1.tzinfo is not None:
                d1 = d1.astimezone(tz=None).replace(tzinfo=None)
            if d2.tzinfo is not None:
                d2 = d2.astimezone(tz=None).replace(tzinfo=None)
            # Check if the time difference is within 24 hours
            if (d2 - d1) <= timedelta(hours=24):
                return d1
            else:
                return pd.NaT
        except Exception:
            return pd.NaT
    else:
        try:
            dt = parser.parse(x)
            if dt.tzinfo is not None:
                dt = dt.astimezone(tz=None).replace(tzinfo=None)
            return dt
        except Exception:
            return pd.NaT

# ==========================================
# Main processing script
# ==========================================

# Load the CSV data (using low_memory=False to avoid dtype warnings)
input_file = "GBIF_species_occurrences_EU_2022-now.csv"
df = pd.read_csv(input_file, low_memory=False)

# Preserve original eventDate strings.
df['raw_eventDate'] = df['eventDate']

# Apply the custom date parser to create a new column, 'parsed_eventDate'.
df['parsed_eventDate'] = df['raw_eventDate'].apply(parse_event_date)

# Identify rows with a date range (containing "/") that failed parsing.
failed_parse = df[df['raw_eventDate'].str.contains("/") & df['parsed_eventDate'].isna()]
print(f"Number of failed date range parses: {len(failed_parse)}")

# Keep only rows with successfully parsed dates.
df_valid = df[df['parsed_eventDate'].notna()].copy()

# Ensure that the 'parsed_eventDate' column is of datetimelike type.
df_valid['parsed_eventDate'] = pd.to_datetime(df_valid['parsed_eventDate'])

# Create a column 'date_str' (formatted as YYYY-MM-DD) for grouping.
df_valid['date_str'] = df_valid['parsed_eventDate'].dt.strftime('%Y-%m-%d')

# Generate the full date range from 2022-01-01 to the latest parsed event date.
start_date = pd.to_datetime('2022-01-01')
end_date = df_valid['parsed_eventDate'].max()
all_dates = pd.date_range(start=start_date, end=end_date).strftime('%Y-%m-%d')

# Group by 'species', 'country', and 'date_str', then count occurrences.
grouped = df_valid.groupby(['species', 'country', 'date_str']).size().reset_index(name='count')

# Pivot the grouped data to wide format:
#   One row per species-country, with one column per date holding the count.
pivot = grouped.pivot_table(index=['species', 'country'],
                            columns='date_str',
                            values='count',
                            fill_value=0).reset_index()

# Ensure that every date in the full timeline is present; add missing date columns as zeros.
for date in all_dates:
    if date not in pivot.columns:
        pivot[date] = 0

# Reorder columns so that static columns come first, then date columns in chronological order.
static_cols = ['species', 'country']
date_cols = sorted([col for col in pivot.columns if col not in static_cols])
pivot = pivot[static_cols + date_cols]

# Rename the columns 'species' to 'Scientific Name' and 'country' to 'Country'
pivot = pivot.rename(columns={'species': 'Scientific Name', 'country': 'Country'})

# Save the resulting pivot table to a CSV file.
output_file = "GBIF_Observations_2022-present_final.csv"
pivot.to_csv(output_file, index=False)
print(f"Saved pivot output to {output_file}")

# Optionally, save the failed date range rows for later examination.
failed_output_file = "failed_parse_dates.csv"
failed_parse.to_csv(failed_output_file, index=False)
print(f"Saved failed date range parses to {failed_output_file}")
