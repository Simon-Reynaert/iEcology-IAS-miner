import pytest
from unittest.mock import patch, Mock
import pandas as pd
import json

# Corrected imports to reference the `activity_mining` package
from src.activity_mining.get_flickr_mentions_final import get_country_from_gps, scrape_flickr_data

# Mock response for the geopy API
MOCK_GEOPY_RESPONSE = Mock()
MOCK_GEOPY_RESPONSE.raw = {
    'address': {
        'country': 'Belgium'
    }
}

# Mock response for when geopy fails
MOCK_GEOPY_FAIL_RESPONSE = Mock(side_effect=Exception("Geopy failed"))

# Mock response from the Flickr API
MOCK_FLICKR_SEARCH_RESPONSE = {
    "photos": {
        "page": 1,
        "pages": 1,
        "perpage": 250,
        "total": 1,
        "photo": [
            {
                "id": "12345",
                "latitude": "50.8503",
                "longitude": "4.3517",
                "tags": "test, tag, ias",
                "datetaken": "2024-01-01 10:00:00",
                "url_o": "https://example.com/photo.jpg"
            }
        ]
    }
}


@patch('src.activity_mining.get_flickr_mentions_final.geolocator.reverse', return_value=MOCK_GEOPY_RESPONSE)
def test_get_country_from_gps_success(mock_reverse):
    """
    Test that the function correctly extracts the country from a successful
    geopy response.
    """
    lat, lon = "50.8503", "4.3517"
    country = get_country_from_gps(lat, lon)
    mock_reverse.assert_called_once_with("50.8503, 4.3517", language='en')
    assert country == "Belgium"


@patch('src.activity_mining.get_flickr_mentions_final.geolocator.reverse', side_effect=MOCK_GEOPY_FAIL_RESPONSE)
def test_get_country_from_gps_failure(mock_reverse):
    """
    Test that the function handles exceptions and returns None on failure.
    """
    lat, lon = "123.456", "789.012"
    country = get_country_from_gps(lat, lon)
    mock_reverse.assert_called_once()
    assert country is None


@patch('src.activity_mining.get_flickr_mentions_final.flickr')
def test_scrape_flickr_data_success(mock_flickr):
    """
    Test the main scraping function by mocking the Flickr API.
    """
    # Configure the mock
    mock_flickr.photos.search.return_value = MOCK_FLICKR_SEARCH_RESPONSE
    
    # Call the main function with mock data
    mock_species_list = ["Axis axis"]
    mock_bounding_boxes = {"EU": (-25, 34, 40, 72)}

    # This assumes `scrape_flickr_data` is refactored to take these arguments.
    # See previous response for the full function signature.
    results_df = scrape_flickr_data(
        flickr_client=mock_flickr,
        geolocator=Mock(), # Always mock the dependencies you pass in!
        species_list=mock_species_list,
        bounding_boxes=mock_bounding_boxes
    )
    
    # Assert that the API was called as expected
    mock_flickr.photos.search.assert_called()
    
    # Assert that the function returned a DataFrame with the correct number of rows
    assert len(results_df) > 0
    assert "photo_id" in results_df.columns