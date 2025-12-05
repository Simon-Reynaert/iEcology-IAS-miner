# tests/test_GBIF_processing.py
import pandas as pd
import pytest
from unittest.mock import patch, MagicMock
import sys
import os

# Add src folder to path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'src')))

from data_processing.process_GBIF_observations import parse_event_date, process_gbif_data

# Constants
INPUT_FILE = "GBIF_species_occurrences_EU.csv"
OUTPUT_FILE = "GBIF_Observations_2016-present_final_test.csv"
FAILED_OUTPUT_FILE = "failed_parse_dates_2016-now_test.csv"


# --- Unit test for parse_event_date ---
def test_parse_event_date():
    """Test the parse_event_date function with various input formats."""
    assert pd.Timestamp('2023-01-01 00:00:00') == parse_event_date('2023-01-01')
    assert pd.Timestamp('2023-01-01 15:30:00') == parse_event_date('2023-01-01T15:30:00')
    assert pd.Timestamp('2023-01-01 00:00:00') == parse_event_date('2023-01-01/2023-01-01T12:00:00')
    assert pd.isna(parse_event_date('2023-01-01/2023-01-02T12:00:00'))  # Range > 24 hours
    assert pd.isna(parse_event_date('Not a date'))
    assert pd.isna(parse_event_date(float('nan')))
    assert pd.isna(parse_event_date(None))


# --- Integration test for process_gbif_data ---
@patch('data_processing.process_GBIF_observations.pd.read_csv')
@patch('data_processing.process_GBIF_observations.tqdm.pandas')
@patch('builtins.print')  # Suppress print statements during testing
def test_process_gbif_data(mock_print, mock_tqdm_pandas, mock_read_csv):
    """Test the complete GBIF data processing pipeline."""
    
    # --- Mock input DataFrame ---
    mock_input_df = pd.DataFrame({
        'species': ['Species A', 'Species A', 'Species B', 'Species A', 'Species B', 'Species C'],
        'country': ['US', 'US', 'US', 'FR', 'FR', 'FR'],
        'eventDate': [
            '2023-01-01', 
            '2023-01-01T12:00:00', 
            '2023-01-01',
            '2023-01-02', 
            '2023-01-02', 
            'Invalid Date'
        ],
        'other_data': ['x', 'y', 'z', 'a', 'b', 'c']
    })
    mock_read_csv.return_value = mock_input_df

    # Make progress_apply redirect to regular apply
    def mock_progress_apply(func, *args, **kwargs):
        # 'self' is bound when this is called as a method
        return mock_input_df["raw_eventDate"].apply(func, *args, **kwargs)
    
    # Patch progress_apply on the Series instance using setattr
    original_apply = pd.Series.apply
    pd.Series.progress_apply = lambda self, func, *args, **kwargs: self.apply(func, *args, **kwargs)
    
    try:
        # --- Run processing ---
        result_df = process_gbif_data(
            input_file=INPUT_FILE,
            output_file=OUTPUT_FILE,
            failed_output_file=FAILED_OUTPUT_FILE,
            start_date='2023-01-01',
            end_date='2023-01-02'
        )

        # --- Assertions ---
        mock_read_csv.assert_called_once_with(INPUT_FILE, low_memory=False)

        # --- Check returned DataFrame structure ---
        assert isinstance(result_df, pd.DataFrame)
        assert 'Scientific Name' in result_df.columns
        assert 'Country' in result_df.columns
        assert '2023-01-01' in result_df.columns
        assert '2023-01-02' in result_df.columns

        # --- Check data integrity ---
        def get_count(df, species, country, date):
            """Helper function to get count for specific species/country/date."""
            row = df[(df['Scientific Name'] == species) & (df['Country'] == country)]
            if row.empty or date not in df.columns:
                return 0
            return int(row[date].iloc[0])

        # Verify observation counts
        assert get_count(result_df, 'Species A', 'US', '2023-01-01') == 2
        assert get_count(result_df, 'Species B', 'US', '2023-01-01') == 1
        assert get_count(result_df, 'Species A', 'FR', '2023-01-02') == 1
        assert get_count(result_df, 'Species B', 'FR', '2023-01-02') == 1
        assert get_count(result_df, 'Species C', 'FR', '2023-01-02') == 0  # Invalid date

        # --- Verify date range completeness ---
        date_columns = [col for col in result_df.columns if col not in ['Scientific Name', 'Country']]
        assert date_columns == ['2023-01-01', '2023-01-02']

        # --- Verify species-country combinations ---
        expected_combinations = [
            ('Species A', 'FR'),
            ('Species A', 'US'),
            ('Species B', 'FR'),
            ('Species B', 'US')
        ]
        actual_combinations = list(result_df[['Scientific Name', 'Country']].itertuples(index=False, name=None))
        assert sorted(actual_combinations) == sorted(expected_combinations)
        
    finally:
        # Clean up: remove progress_apply
        if hasattr(pd.Series, 'progress_apply'):
            delattr(pd.Series, 'progress_apply')


def test_process_gbif_data_file_not_found():
    """Test that process_gbif_data handles missing files gracefully."""
    with patch('builtins.print'):
        result = process_gbif_data(
            input_file="nonexistent_file.csv",
            output_file=OUTPUT_FILE
        )
    
    assert result.empty


def test_parse_event_date_timezone_handling():
    """Test that timezone information is properly removed."""
    # Date with timezone
    date_with_tz = '2023-01-01T12:00:00+02:00'
    parsed = parse_event_date(date_with_tz)
    
    assert not pd.isna(parsed)
    assert parsed.tzinfo is None  # Should be timezone-naive


def test_parse_event_date_edge_cases():
    """Test edge cases for date range parsing."""
    # Exactly 24 hours - should be accepted
    assert not pd.isna(parse_event_date('2023-01-01T00:00:00/2023-01-02T00:00:00'))
    
    # Just over 24 hours - should be rejected
    assert pd.isna(parse_event_date('2023-01-01T00:00:00/2023-01-02T00:00:01'))
    
    # Malformed range
    assert pd.isna(parse_event_date('2023-01-01/2023-01-02/2023-01-03'))