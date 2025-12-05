import pandas as pd
import pytest
from unittest.mock import patch, MagicMock
import os
import sys

# Add the parent directory to the Python path for module discovery
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..')))

# Import the refactored module
from src.data_processing.process_flickr_images import (
    cc_to_name, 
    haversine, 
    process_flickr_data,  # updated function name
    EU_SCHENGEN_EFTA_COUNTRIES
)

## Unit Tests

def test_cc_to_name_valid():
    """Test converting a valid country code to its name."""
    assert cc_to_name('DE') == 'Germany'
    assert cc_to_name('US') == 'United States'
    
def test_cc_to_name_invalid():
    """Test that an invalid country code returns the code itself."""
    assert cc_to_name('XX') == 'XX'

def test_haversine():
    """Test the haversine distance calculation."""
    assert haversine(50.0, 10.0, 50.0, 10.0) == 0.0

    distance = haversine(40.7128, -74.0060, 40.7128, -74.0050)
    assert 50 < distance < 150

    distance_london_paris = haversine(51.5074, -0.1278, 48.8566, 2.3522)
    assert 340000 < distance_london_paris < 350000

## Integration Test for process_flickr_data
@patch('src.data_processing.process_flickr_images.pd.read_csv')
@patch('src.data_processing.process_flickr_images.rg_search')
@patch('src.data_processing.process_flickr_images.tqdm')
def test_process_flickr_data(mock_tqdm, mock_rg_search, mock_read_csv):
    """Test the full workflow of the process_flickr_data function using mocks."""
    mock_data = {
        'latitude': [51.5074, 51.5075, 48.8566, 40.7128, 40.7128],
        'longitude': [-0.1278, -0.1279, 2.3522, -74.0060, -74.0060],
        'date_taken': [
            '2023-01-01 10:00:00', '2023-01-01 10:00:01', 
            '2023-01-02 11:00:00', '2023-01-03 12:00:00', '2023-01-03 12:30:00'
        ],
        'tags': ['bird tag1 tag2', 'bird tag1 tag3', 'cat', 'dog tag_a tag_b', 'dog tag_c tag_b']
    }
    
    mock_df = pd.DataFrame(mock_data)
    mock_read_csv.return_value = mock_df

    mock_rg_search.return_value = [
        {'cc': 'GB', 'name': 'United Kingdom'},
        {'cc': 'GB', 'name': 'United Kingdom'},
        {'cc': 'FR', 'name': 'France'},
        {'cc': 'US', 'name': 'United States'},
        {'cc': 'US', 'name': 'United States'}
    ]

    mock_tqdm.return_value.__enter__.return_value = MagicMock()
    
    captured_df = None

    def capture_to_csv(self, *args, **kwargs):
        nonlocal captured_df
        captured_df = self.copy()
        return None

    with patch.object(pd.DataFrame, 'to_csv', new=capture_to_csv):
        process_flickr_data()  # run the refactored function

        mock_read_csv.assert_called_once_with('flickr_species_observations_eu_combined_latin_normtag_2004-now.csv')
        assert captured_df is not None
        assert 'detected_cc' in captured_df.columns
        assert 'detected_country' in captured_df.columns

        european_countries = set(captured_df['detected_cc'].unique())
        assert european_countries.issubset(EU_SCHENGEN_EFTA_COUNTRIES)
        assert len(captured_df) > 0
        assert mock_rg_search.called
        coords_called = mock_rg_search.call_args[0][0]
        assert len(coords_called) > 0

        print(f"Test results: {len(captured_df)} rows, countries: {european_countries}")
