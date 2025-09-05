import pandas as pd
import pytest
from unittest.mock import patch, MagicMock
import os
import sys

# Add the parent directory to the Python path for module discovery
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..')))

# Now the import path should be correct
from src.data_processing.clean_geolocate_summarize_flickr_normtags import (
    cc_to_name, 
    haversine, 
    main, 
    EU_SCHENGEN_EFTA_COUNTRIES, 
    DISTANCE_THRESHOLD_METERS
)

## Unit Tests
# These tests are correct and do not need to be changed.

def test_cc_to_name_valid():
    """Test converting a valid country code to its name."""
    assert cc_to_name('DE') == 'Germany'
    assert cc_to_name('US') == 'United States'
    
def test_cc_to_name_invalid():
    """Test that an invalid country code returns the code itself."""
    assert cc_to_name('XX') == 'XX'

def test_haversine():
    """Test the haversine distance calculation."""
    # Two identical points should have a distance of 0.
    assert haversine(50.0, 10.0, 50.0, 10.0) == 0.0

    # A short, non-zero distance. This is an approximate check.
    # New York City to a point very close by.
    distance = haversine(40.7128, -74.0060, 40.7128, -74.0050)
    assert 50 < distance < 150

    # A known longer distance between two major cities (London and Paris).
    # Expected value is around 343km or 343,000 meters.
    distance_london_paris = haversine(51.5074, -0.1278, 48.8566, 2.3522)
    assert 340000 < distance_london_paris < 350000

## Integration Test for Main Function
@patch('src.data_processing.clean_geolocate_summarize_flickr_normtags.pd.read_csv')
@patch('src.data_processing.clean_geolocate_summarize_flickr_normtags.rg_search')
@patch('src.data_processing.clean_geolocate_summarize_flickr_normtags.tqdm')
def test_main_functionality(mock_tqdm, mock_rg_search, mock_read_csv):
    """
    Test the full workflow of the main function using mocks.
    """
    # Create a dummy DataFrame to be returned by mock_read_csv
    mock_data = {
        'latitude': [51.5074, 51.5075, 48.8566, 40.7128, 40.7128],
        'longitude': [-0.1278, -0.1279, 2.3522, -74.0060, -74.0060],
        'date_taken': ['2023-01-01 10:00:00', '2023-01-01 10:00:01', '2023-01-02 11:00:00', '2023-01-03 12:00:00', '2023-01-03 12:30:00'],
        'tags': ['bird tag1 tag2', 'bird tag1 tag3', 'cat', 'dog tag_a tag_b', 'dog tag_c tag_b']
    }
    
    mock_df = pd.DataFrame(mock_data)
    mock_read_csv.return_value = mock_df

    # Configure the mock reverse geocoder
    # We need to return enough results for all rows that survive deduplication
    mock_rg_search.return_value = [
        {'cc': 'GB', 'name': 'United Kingdom'},
        {'cc': 'GB', 'name': 'United Kingdom'},  
        {'cc': 'FR', 'name': 'France'},
        {'cc': 'US', 'name': 'United States'},
        {'cc': 'US', 'name': 'United States'}
    ]

    # Mock the tqdm progress bar
    mock_tqdm.return_value.__enter__.return_value = MagicMock()
    
    # Variable to store the DataFrame that gets saved
    captured_df = None
    
    # Create a replacement function for to_csv that captures the DataFrame
    def capture_to_csv(self, *args, **kwargs):
        nonlocal captured_df
        captured_df = self.copy()  # Capture the DataFrame instance
        return None  # Don't actually write to file
    
    # Patch the to_csv method directly using 'new=' (NOT 'side_effect=')
    with patch.object(pd.DataFrame, 'to_csv', new=capture_to_csv):
        # Run the main function
        main()

        # Verify that the correct input file was read
        mock_read_csv.assert_called_once_with('flickr_species_observations_eu_combined_latin_normtag_2004-now.csv')
        
        # Verify the captured DataFrame
        assert captured_df is not None, "DataFrame was not captured"
        
        # Verify required columns exist
        assert 'detected_cc' in captured_df.columns
        assert 'detected_country' in captured_df.columns
        
        # Verify only European countries are included (GB and FR from our mock data)
        european_countries = set(captured_df['detected_cc'].unique())
        assert european_countries.issubset(EU_SCHENGEN_EFTA_COUNTRIES)
        
        # Based on the mock data and deduplication logic, we should have some rows
        # The exact number depends on the deduplication algorithm
        assert len(captured_df) > 0
        
        # Verify that geocoding was called
        assert mock_rg_search.called
        
        # The geocoding should have been called with coordinates
        coords_called = mock_rg_search.call_args[0][0]
        assert len(coords_called) > 0
        
        print(f"Test results: {len(captured_df)} rows, countries: {european_countries}")