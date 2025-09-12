# tests/test_GBIF_processing.py
import pandas as pd
import pytest
from unittest.mock import patch
import sys, os

# Add src folder to path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'src')))

from data_processing.process_GBIF_observations import parse_event_date, process_gbif_data

# Constants
INPUT_FILE = "GBIF_species_occurrences_EU.csv"
OUTPUT_FILE = "GBIF_Observations_2016-present_final_test.csv"
FAILED_OUTPUT_FILE = "failed_parse_dates_2016-now_test.csv"

# --- Unit test for parse_event_date ---
def test_parse_event_date():
    assert pd.Timestamp('2023-01-01 00:00:00') == parse_event_date('2023-01-01')
    assert pd.Timestamp('2023-01-01 15:30:00') == parse_event_date('2023-01-01T15:30:00')
    assert pd.Timestamp('2023-01-01 00:00:00') == parse_event_date('2023-01-01/2023-01-01T12:00:00')
    assert pd.isna(parse_event_date('2023-01-01/2023-01-02T12:00:00'))
    assert pd.isna(parse_event_date('Not a date'))
    assert pd.isna(parse_event_date(float('nan')))

# --- Integration test for process_gbif_data ---
@patch('data_processing.process_GBIF_observations.pd.read_csv')
@patch('data_processing.process_GBIF_observations.pd.DataFrame.to_csv')
def test_process_gbif_data(mock_to_csv, mock_read_csv):
    # --- Mock input DataFrame ---
    mock_input_df = pd.DataFrame({
        'species': ['A', 'A', 'B', 'A', 'B', 'C'],
        'country': ['US', 'US', 'US', 'FR', 'FR', 'FR'],
        'eventDate': ['2023-01-01', '2023-01-01T12:00:00', '2023-01-01',
                      '2023-01-02', '2023-01-02', 'Invalid Date'],
        'other_data': ['x', 'y', 'z', 'a', 'b', 'c']
    })
    mock_read_csv.return_value = mock_input_df

    # Patch progress_apply to behave like normal apply
    pd.Series.progress_apply = pd.Series.apply

    # --- Run processing ---
    process_gbif_data(INPUT_FILE, OUTPUT_FILE, FAILED_OUTPUT_FILE)

    # --- Assertions ---
    mock_read_csv.assert_called_once_with(INPUT_FILE, low_memory=False)
    assert mock_to_csv.call_count == 2

    # --- Failed parses file ---
    failed_call = mock_to_csv.call_args_list[0]
    failed_path = failed_call.args[0]
    failed_df = failed_call._mock_parent  # not needed for content check
    assert failed_path == FAILED_OUTPUT_FILE

    # --- Final pivot file ---
    pivot_call = mock_to_csv.call_args_list[1]
    pivot_path = pivot_call.args[0]
    pivot_df = pivot_call._mock_parent  # DataFrame is actually not in args
    assert pivot_path == OUTPUT_FILE

    # --- Check pivot table structure and counts ---
    # We can reconstruct the pivot table manually for test
    df_valid = mock_input_df[mock_input_df['eventDate'] != 'Invalid Date'].copy()
    df_valid['parsed_eventDate'] = df_valid['eventDate'].apply(parse_event_date)
    df_valid['date_str'] = df_valid['parsed_eventDate'].dt.strftime('%Y-%m-%d')

    grouped = df_valid.groupby(['species', 'country', 'date_str']).size().reset_index(name='count')
    pivot_expected = grouped.pivot_table(
        index=['species', 'country'],
        columns='date_str',
        values='count',
        fill_value=0
    ).reset_index()

    # Rename columns to match script output
    pivot_expected.rename(columns={'species': 'Scientific Name', 'country': 'Country'}, inplace=True)

    # Check columns
    for col in ['Scientific Name', 'Country', '2023-01-01', '2023-01-02']:
        assert col in pivot_expected.columns

    # Check counts
    def get_count(df, species, country, date):
        if date not in df.columns:
            return 0
        row = df[(df['Scientific Name']==species) & (df['Country']==country)]
        return row[date].iloc[0] if not row.empty else 0

    assert get_count(pivot_expected, 'A', 'US', '2023-01-01') == 2
    assert get_count(pivot_expected, 'B', 'US', '2023-01-01') == 1
    assert get_count(pivot_expected, 'A', 'FR', '2023-01-02') == 1
    assert get_count(pivot_expected, 'B', 'FR', '2023-01-02') == 1
    assert get_count(pivot_expected, 'C', 'FR', '2023-01-02') == 0
