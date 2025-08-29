import pytest
from unittest.mock import patch, Mock
import pandas as pd

# Import the functions from your refactored module
from src.activity_mining.get_flickr_mentions_final import (
    get_country_from_gps,
    scrape_flickr_data
)

# -------------------------------
# Mock data
# -------------------------------

# Mock geopy response (successful)
MOCK_GEOPY_RESPONSE = Mock()
MOCK_GEOPY_RESPONSE.raw = {'address': {'country': 'Belgium'}}

# Mock geopy failure
MOCK_GEOPY_FAIL_RESPONSE = Mock(side_effect=Exception("Geopy failed"))

# Mock Flickr API response
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

# -------------------------------
# Tests for get_country_from_gps
# -------------------------------

@patch('src.activity_mining.get_flickr_mentions_final.geolocator.reverse', return_value=MOCK_GEOPY_RESPONSE)
def test_get_country_from_gps_success(mock_reverse):
    lat, lon = "50.8503", "4.3517"
    country = get_country_from_gps(lat, lon)
    mock_reverse.assert_called_once_with("50.8503, 4.3517", language='en')
    assert country == "Belgium"

@patch('src.activity_mining.get_flickr_mentions_final.geolocator.reverse', side_effect=MOCK_GEOPY_FAIL_RESPONSE)
def test_get_country_from_gps_failure(mock_reverse):
    lat, lon = "123.456", "789.012"
    country = get_country_from_gps(lat, lon)
    mock_reverse.assert_called_once()
    assert country is None

# -------------------------------
# Test for scrape_flickr_data
# -------------------------------

@patch('src.activity_mining.get_flickr_mentions_final.get_flickr_client')
def test_scrape_flickr_data_success(mock_get_flickr_client):
    """
    Test scraping function by mocking both the Flickr client and geolocator.
    """
    # Mock the Flickr client returned by get_flickr_client()
    mock_flickr_client = Mock()
    mock_flickr_client.photos.search.return_value = MOCK_FLICKR_SEARCH_RESPONSE
    mock_get_flickr_client.return_value = mock_flickr_client

    # Mock species list and bounding boxes
    mock_species_list = ["Axis axis"]
    mock_bounding_boxes = {"EU": (-25, 34, 40, 72)}

    # Call the scraper
    results_df = scrape_flickr_data(
        flickr_client=mock_flickr_client,
        geolocator=Mock(),  # mock geolocator
        species_list=mock_species_list,
        bounding_boxes=mock_bounding_boxes
    )

    # Check that Flickr API was called
    mock_flickr_client.photos.search.assert_called()
    # Check DataFrame output
    assert isinstance(results_df, pd.DataFrame)
    assert len(results_df) == 1
    assert "photo_id" in results_df.columns
    assert results_df.iloc[0]["scientific_name"] == "Axis axis"
    assert results_df.iloc[0]["latitude"] == "50.8503"
    assert results_df.iloc[0]["longitude"] == "4.3517"
