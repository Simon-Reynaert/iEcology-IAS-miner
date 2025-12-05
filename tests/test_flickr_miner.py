import pytest
from unittest.mock import patch, Mock, MagicMock
import pandas as pd
import numpy as np

# Import the functions from your refactored module
# Update this import path to match your actual file location
from src.activity_mining.get_flickr_mentions_final import (
    get_country_from_gps_fast,
    scrape_flickr_data,
    get_exif_coords
)

# -------------------------------
# Mock data
# -------------------------------

# Mock reverse_geocoder response (successful)
MOCK_RG_RESPONSE = [
    {
        'cc': 'BE',  # Country code
        'name': 'Brussels',
        'admin1': 'Brussels Capital',
        'admin2': '',
        'lat': '50.85045',
        'lon': '4.34878'
    }
]

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

# Mock Flickr API response with missing GPS data
MOCK_FLICKR_NO_GPS_RESPONSE = {
    "photos": {
        "page": 1,
        "pages": 1,
        "perpage": 250,
        "total": 1,
        "photo": [
            {
                "id": "67890",
                "latitude": "0",  # Missing GPS
                "longitude": "0",
                "tags": "test, tag",
                "datetaken": "2024-01-01 10:00:00",
                "url_o": "https://example.com/photo2.jpg"
            }
        ]
    }
}

# Mock EXIF response with GPS data
MOCK_EXIF_WITH_GPS = {
    "photo": {
        "exif": [
            {
                "label": "GPS Latitude",
                "raw": {"_content": "51.5074"}
            },
            {
                "label": "GPS Longitude",
                "raw": {"_content": "-0.1278"}
            },
            {
                "label": "Camera Model",
                "raw": {"_content": "Canon EOS"}
            }
        ]
    }
}

# Mock EXIF response without GPS data
MOCK_EXIF_NO_GPS = {
    "photo": {
        "exif": [
            {
                "label": "Camera Model",
                "raw": {"_content": "Canon EOS"}
            }
        ]
    }
}

# -------------------------------
# Tests for get_country_from_gps_fast
# -------------------------------

@patch('src.activity_mining.get_flickr_mentions_final.rg.search', return_value=MOCK_RG_RESPONSE)
def test_get_country_from_gps_fast_success(mock_rg_search):
    """Test successful reverse geocoding with valid coordinates."""
    lat, lon = "50.8503", "4.3517"
    country = get_country_from_gps_fast(lat, lon)
    
    # Check that reverse_geocoder was called with correct parameters
    mock_rg_search.assert_called_once_with([(50.8503, 4.3517)], mode=1)
    assert country == "BE"


@patch('src.activity_mining.get_flickr_mentions_final.rg.search', return_value=MOCK_RG_RESPONSE)
def test_get_country_from_gps_fast_numeric_input(mock_rg_search):
    """Test that numeric inputs (not strings) work correctly."""
    lat, lon = 50.8503, 4.3517
    country = get_country_from_gps_fast(lat, lon)
    
    mock_rg_search.assert_called_once_with([(50.8503, 4.3517)], mode=1)
    assert country == "BE"


def test_get_country_from_gps_fast_invalid_coordinates():
    """Test handling of invalid coordinate formats."""
    # Test with non-numeric strings
    country = get_country_from_gps_fast("invalid", "coords")
    assert country is None
    
    # Test with None values
    country = get_country_from_gps_fast(None, None)
    assert country is None


def test_get_country_from_gps_fast_zero_coordinates():
    """Test that (0, 0) coordinates are rejected as invalid."""
    country = get_country_from_gps_fast(0.0, 0.0)
    assert country is None
    
    country = get_country_from_gps_fast("0", "0")
    assert country is None


def test_get_country_from_gps_fast_nan_coordinates():
    """Test handling of NaN and infinity values."""
    # Test with NaN
    country = get_country_from_gps_fast(np.nan, 4.3517)
    assert country is None
    
    # Test with infinity
    country = get_country_from_gps_fast(np.inf, 4.3517)
    assert country is None


@patch('src.activity_mining.get_flickr_mentions_final.rg.search', side_effect=Exception("Geocoding failed"))
def test_get_country_from_gps_fast_exception_handling(mock_rg_search):
    """Test that exceptions during geocoding are handled gracefully."""
    lat, lon = "50.8503", "4.3517"
    country = get_country_from_gps_fast(lat, lon)
    
    mock_rg_search.assert_called_once()
    assert country is None


@patch('src.activity_mining.get_flickr_mentions_final.rg.search', return_value=[{}])
def test_get_country_from_gps_fast_no_country_code(mock_rg_search):
    """Test handling when geocoder returns result without country code."""
    lat, lon = "50.8503", "4.3517"
    country = get_country_from_gps_fast(lat, lon)
    
    mock_rg_search.assert_called_once()
    assert country is None


# -------------------------------
# Tests for get_exif_coords
# -------------------------------

def test_get_exif_coords_success():
    """Test successful EXIF coordinate extraction."""
    mock_flickr_client = Mock()
    mock_flickr_client.photos.getExif.return_value = MOCK_EXIF_WITH_GPS
    
    lat, lon = get_exif_coords(mock_flickr_client, "12345")
    
    mock_flickr_client.photos.getExif.assert_called_once_with(photo_id="12345")
    assert lat == "51.5074"
    assert lon == "-0.1278"


def test_get_exif_coords_no_gps():
    """Test EXIF extraction when GPS data is missing."""
    mock_flickr_client = Mock()
    mock_flickr_client.photos.getExif.return_value = MOCK_EXIF_NO_GPS
    
    lat, lon = get_exif_coords(mock_flickr_client, "12345")
    
    mock_flickr_client.photos.getExif.assert_called_once()
    assert lat is None
    assert lon is None


def test_get_exif_coords_api_failure():
    """Test EXIF extraction when API call fails."""
    mock_flickr_client = Mock()
    mock_flickr_client.photos.getExif.side_effect = Exception("API Error")
    
    lat, lon = get_exif_coords(mock_flickr_client, "12345")
    
    mock_flickr_client.photos.getExif.assert_called_once()
    assert lat is None
    assert lon is None


# -------------------------------
# Tests for scrape_flickr_data
# -------------------------------

@patch('src.activity_mining.get_flickr_mentions_final.get_country_from_gps_fast')
@patch('src.activity_mining.get_flickr_mentions_final.time.sleep')  # Mock sleep to speed up tests
def test_scrape_flickr_data_success(mock_sleep, mock_get_country):
    """Test scraping function with valid Flickr response."""
    # Setup mocks
    mock_flickr_client = Mock()
    mock_flickr_client.photos.search.return_value = MOCK_FLICKR_SEARCH_RESPONSE
    mock_get_country.return_value = "BE"
    
    # Mock species list and bounding boxes
    mock_species_list = ["Axis axis"]
    mock_bounding_boxes = {"EU": (-25, 34, 40, 72)}
    
    # Call the scraper
    results_df = scrape_flickr_data(
        flickr_client=mock_flickr_client,
        species_list=mock_species_list,
        bounding_boxes=mock_bounding_boxes,
        start_date="2024-01-01",
        end_date="2024-12-31"
    )
    
    # Assertions
    mock_flickr_client.photos.search.assert_called()
    mock_get_country.assert_called_once_with("50.8503", "4.3517")
    
    # Check DataFrame output
    assert isinstance(results_df, pd.DataFrame)
    assert len(results_df) == 1
    assert "photo_id" in results_df.columns
    assert "scientific_name" in results_df.columns
    assert "country" in results_df.columns
    assert "latitude" in results_df.columns
    assert "longitude" in results_df.columns
    
    # Check specific values
    assert results_df.iloc[0]["photo_id"] == "12345"
    assert results_df.iloc[0]["scientific_name"] == "Axis axis"
    assert results_df.iloc[0]["latitude"] == "50.8503"
    assert results_df.iloc[0]["longitude"] == "4.3517"
    assert results_df.iloc[0]["country"] == "BE"


@patch('src.activity_mining.get_flickr_mentions_final.get_exif_coords')
@patch('src.activity_mining.get_flickr_mentions_final.get_country_from_gps_fast')
@patch('src.activity_mining.get_flickr_mentions_final.time.sleep')
def test_scrape_flickr_data_exif_fallback(mock_sleep, mock_get_country, mock_get_exif):
    """Test that EXIF fallback is used when main GPS data is missing."""
    # Setup mocks
    mock_flickr_client = Mock()
    mock_flickr_client.photos.search.return_value = MOCK_FLICKR_NO_GPS_RESPONSE
    mock_get_exif.return_value = ("51.5074", "-0.1278")  # EXIF provides coords
    mock_get_country.return_value = "GB"
    
    mock_species_list = ["Test species"]
    mock_bounding_boxes = {"EU": (-25, 34, 40, 72)}
    
    # Call the scraper
    results_df = scrape_flickr_data(
        flickr_client=mock_flickr_client,
        species_list=mock_species_list,
        bounding_boxes=mock_bounding_boxes
    )
    
    # Check that EXIF fallback was called
    mock_get_exif.assert_called_once_with(mock_flickr_client, "67890")
    
    # Check that geocoding used the EXIF coordinates
    mock_get_country.assert_called_once_with("51.5074", "-0.1278")
    
    # Check DataFrame
    assert len(results_df) == 1
    assert results_df.iloc[0]["latitude"] == "51.5074"
    assert results_df.iloc[0]["longitude"] == "-0.1278"


@patch('src.activity_mining.get_flickr_mentions_final.get_exif_coords')
@patch('src.activity_mining.get_flickr_mentions_final.time.sleep')
def test_scrape_flickr_data_skip_ungeolocated(mock_sleep, mock_get_exif):
    """Test that photos without GPS data are skipped."""
    # Setup mocks
    mock_flickr_client = Mock()
    mock_flickr_client.photos.search.return_value = MOCK_FLICKR_NO_GPS_RESPONSE
    mock_get_exif.return_value = (None, None)  # EXIF also fails
    
    mock_species_list = ["Test species"]
    mock_bounding_boxes = {"EU": (-25, 34, 40, 72)}
    
    # Call the scraper
    results_df = scrape_flickr_data(
        flickr_client=mock_flickr_client,
        species_list=mock_species_list,
        bounding_boxes=mock_bounding_boxes
    )
    
    # Check that photo was skipped (no results)
    assert len(results_df) == 0


@patch('src.activity_mining.get_flickr_mentions_final.get_country_from_gps_fast')
@patch('src.activity_mining.get_flickr_mentions_final.time.sleep')
def test_scrape_flickr_data_geocoding_fallback(mock_sleep, mock_get_country):
    """Test fallback to region name when geocoding fails."""
    # Setup mocks
    mock_flickr_client = Mock()
    mock_flickr_client.photos.search.return_value = MOCK_FLICKR_SEARCH_RESPONSE
    mock_get_country.return_value = None  # Geocoding fails
    
    mock_species_list = ["Test species"]
    mock_bounding_boxes = {"EU": (-25, 34, 40, 72)}
    
    # Call the scraper
    results_df = scrape_flickr_data(
        flickr_client=mock_flickr_client,
        species_list=mock_species_list,
        bounding_boxes=mock_bounding_boxes
    )
    
    # Check that region name "EU" was used as fallback
    assert len(results_df) == 1
    assert results_df.iloc[0]["country"] == "EU"


@patch('src.activity_mining.get_flickr_mentions_final.time.sleep')
def test_scrape_flickr_data_api_error(mock_sleep):
    """Test handling of Flickr API errors."""
    # Setup mock that raises exception
    mock_flickr_client = Mock()
    mock_flickr_client.photos.search.side_effect = Exception("API Error")
    
    mock_species_list = ["Test species"]
    mock_bounding_boxes = {"EU": (-25, 34, 40, 72)}
    
    # Call the scraper - should not crash
    results_df = scrape_flickr_data(
        flickr_client=mock_flickr_client,
        species_list=mock_species_list,
        bounding_boxes=mock_bounding_boxes
    )
    
    # Should return empty DataFrame
    assert len(results_df) == 0


@patch('src.activity_mining.get_flickr_mentions_final.get_country_from_gps_fast')
@patch('src.activity_mining.get_flickr_mentions_final.time.sleep')
def test_scrape_flickr_data_multiple_species(mock_sleep, mock_get_country):
    """Test scraping with multiple species."""
    # Setup mocks
    mock_flickr_client = Mock()
    mock_flickr_client.photos.search.return_value = MOCK_FLICKR_SEARCH_RESPONSE
    mock_get_country.return_value = "BE"
    
    mock_species_list = ["Species A", "Species B", "Species C"]
    mock_bounding_boxes = {"EU": (-25, 34, 40, 72)}
    
    # Call the scraper
    results_df = scrape_flickr_data(
        flickr_client=mock_flickr_client,
        species_list=mock_species_list,
        bounding_boxes=mock_bounding_boxes
    )
    
    # Should have called Flickr API once per species
    assert mock_flickr_client.photos.search.call_count == 3
    
    # Should have 3 results (one per species)
    assert len(results_df) == 3


@patch('src.activity_mining.get_flickr_mentions_final.get_country_from_gps_fast')
@patch('src.activity_mining.get_flickr_mentions_final.time.sleep')
def test_scrape_flickr_data_date_range(mock_sleep, mock_get_country):
    """Test that date range parameters are passed correctly."""
    # Setup mocks
    mock_flickr_client = Mock()
    mock_flickr_client.photos.search.return_value = MOCK_FLICKR_SEARCH_RESPONSE
    mock_get_country.return_value = "BE"
    
    mock_species_list = ["Test species"]
    mock_bounding_boxes = {"EU": (-25, 34, 40, 72)}
    
    # Call with specific date range
    results_df = scrape_flickr_data(
        flickr_client=mock_flickr_client,
        species_list=mock_species_list,
        bounding_boxes=mock_bounding_boxes,
        start_date="2023-01-01",
        end_date="2023-12-31"
    )
    
    # Check that the API was called with correct date parameters
    call_args = mock_flickr_client.photos.search.call_args
    assert call_args.kwargs['min_taken_date'] == "2023-01-01"
    assert call_args.kwargs['max_taken_date'] == "2023-12-31"