# test_youtube_miner.py

import pandas as pd
import pytest
import os
from unittest.mock import patch, MagicMock
from datetime import datetime, timedelta
from io import StringIO
import sys

# Add the parent directory to the path to find your 'src' folder
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

# Correct import path assuming your script is in a 'src' directory
from src.activity_mining.get_youtube_mentions_final import (
    load_progress,
    update_progress,
    search_youtube_geo,
    fetch_videos,
    split_time_range,
    save_progress,
    get_video_data_for_species_and_countries
)

# --- Mock Data ---

MOCK_PROGRESS_CSV_CONTENT = "species,country,timestamp\nTest Species,Test Country,2024-01-01 10:00:00\n"

MOCK_SPECIES_CSV_CONTENT = """Scientific Name,Other Column
Species A,Data
Species B,Data
Species C,Data
"""

MOCK_YT_RESPONSE_PAGE1 = {
    'items': [
        {'id': {'kind': 'youtube#video', 'videoId': 'vid1'}, 'snippet': {'title': 'Title 1', 'description': 'desc', 'publishedAt': '2024-01-01', 'channelTitle': 'Chan1'}},
        {'id': {'kind': 'youtube#video', 'videoId': 'vid2'}, 'snippet': {'title': 'Title 2', 'description': 'desc', 'publishedAt': '2024-01-02', 'channelTitle': 'Chan2'}},
    ],
    'nextPageToken': 'test_token'
}

MOCK_YT_RESPONSE_PAGE2 = {
    'items': [
        {'id': {'kind': 'youtube#video', 'videoId': 'vid3'}, 'snippet': {'title': 'Title 3', 'description': 'desc', 'publishedAt': '2024-01-03', 'channelTitle': 'Chan3'}},
    ],
    'nextPageToken': None
}

MOCK_YT_RESPONSE_CAP_HIT = {
    'items': [
        {'id': {'kind': 'youtube#video', 'videoId': f'vid{i}'}, 'snippet': {'title': f'Video {i}', 'description': 'desc', 'publishedAt': '2024-01-01', 'channelTitle': 'Chan'}} for i in range(50)
    ],
    'nextPageToken': 'cap_token'
}

# --- Fixtures for Mocking ---

@pytest.fixture
def mock_youtube_client():
    """Fixture to create a mock YouTube API client."""
    mock_client = MagicMock()
    mock_search = mock_client.search.return_value
    mock_search.list.return_value.execute.return_value = MOCK_YT_RESPONSE_PAGE1
    return mock_client

@pytest.fixture(autouse=True)
def mock_all_globals(monkeypatch):
    """Mocks global variables and external dependencies at the module level."""
    # Mocking external dependencies
    monkeypatch.setattr('src.activity_mining.get_youtube_mentions_final.load_dotenv', MagicMock())
    monkeypatch.setattr(os, 'getenv', lambda key: 'FAKE_API_KEY')
    monkeypatch.setattr(os, 'makedirs', MagicMock())

@pytest.fixture
def mock_file_io(monkeypatch):
    """Mock file reading/writing for load_progress and save_progress."""
    import pandas as pd

    MOCK_PROGRESS_CSV_CONTENT = [
        {"species": "Test Species", "country": "Test Country", "timestamp": "2024-01-01 10:00:00"}
    ]

    MOCK_SPECIES_CSV_CONTENT = [
        {"Scientific Name": "Species A", "Other Column": "Data"},
        {"Scientific Name": "Species B", "Other Column": "Data"},
        {"Scientific Name": "Species C", "Other Column": "Data"}
    ]

    # Patch os.path.exists
    monkeypatch.setattr(os.path, 'exists', lambda path: path in ['mock_progress.csv', 'list_of_union_concern.csv'])

    # Patch pd.read_csv to return a real DataFrame
    def mock_read_csv(path, *args, **kwargs):
        if path == 'mock_progress.csv':
            return pd.DataFrame(MOCK_PROGRESS_CSV_CONTENT)
        elif path == 'list_of_union_concern.csv':
            return pd.DataFrame(MOCK_SPECIES_CSV_CONTENT)
        else:
            return pd.DataFrame()

    monkeypatch.setattr(pd, 'read_csv', mock_read_csv)

    # Patch to_csv so no files are written
    monkeypatch.setattr(pd.DataFrame, 'to_csv', MagicMock())
    
# --- Tests for each function ---

# test_load_progress_exists
def test_load_progress_exists(mock_file_io):
    """Test loading progress when the file exists."""
    df = load_progress('mock_progress.csv')

    # Now should contain the row from MOCK_PROGRESS_CSV_CONTENT
    assert not df.empty
    assert df.iloc[0]['species'] == 'Test Species'
    assert df.iloc[0]['country'] == 'Test Country'
    assert 'timestamp' in df.columns

def test_load_progress_not_exists(monkeypatch):
    """Test loading progress when the file does not exist."""
    # Patch os.path.exists to always return False
    monkeypatch.setattr(os.path, 'exists', lambda path: False)

    df = load_progress('mock_progress.csv')

    # Should be empty but have the correct columns
    assert df.empty
    assert list(df.columns) == ["species", "country", "timestamp"]

# test_update_progress
@patch('pandas.DataFrame')
def test_update_progress(mock_df_class, monkeypatch):
    """Test that update_progress correctly appends and saves a new row."""
    # Create a mock DataFrame instance
    mock_df_instance = MagicMock()
    mock_df_class.return_value = mock_df_instance

    # Patch pandas.concat to return a real DataFrame with our test data
    monkeypatch.setattr('pandas.concat', lambda dfs, ignore_index: pd.DataFrame({
        "species": ["New Species"],
        "country": ["New Country"],
        "timestamp": ["2024-01-01"]
    }))

    # Call the function under test
    update_progress("New Species", "New Country", 'mock_progress.csv')

    # Ensure to_csv was called on the DataFrame instance
    mock_df_instance.to_csv.assert_called_once()


def test_search_youtube_geo_success(mock_youtube_client):
    """Test successful API call to search_youtube_geo."""
    # This test now uses the mock_youtube_client fixture
    response = search_youtube_geo(mock_youtube_client, "query", "loc", "rad", "after", "before")
    
    # The assertions should check if the mock was used
    mock_youtube_client.search.return_value.list.return_value.execute.assert_called_once()
    assert 'items' in response

@patch('src.activity_mining.get_youtube_mentions_final.search_youtube_geo')
def test_fetch_videos_pagination(mock_search_youtube_geo):
    """Test that fetch_videos handles pagination correctly."""
    mock_search_youtube_geo.side_effect = [MOCK_YT_RESPONSE_PAGE1, MOCK_YT_RESPONSE_PAGE2]
    
    # Passing a mock object to the function
    video_data, fetched_count = fetch_videos(MagicMock(), "query", {"country": "test", "location": "loc", "radius": "rad"}, "after", "before")

    assert fetched_count == 3
    assert len(video_data) == 3
    assert video_data[2]['video_id'] == 'vid3'

def test_split_time_range():
    """Test the time range splitting logic."""
    start = datetime(2020, 1, 1)
    end = datetime(2020, 3, 1)
    intervals = split_time_range(start, end, delta_days=30)
    assert len(intervals) == 2
    assert intervals[0][0] == '2020-01-01T00:00:00Z'
    assert intervals[1][1] == '2020-03-01T00:00:00Z'

# test_save_progress_new_file
@patch('src.activity_mining.get_youtube_mentions_final.os.path.exists', return_value=False)
@patch('src.activity_mining.get_youtube_mentions_final.os.makedirs')
def test_save_progress_new_file(mock_makedirs, mock_exists):
    """Test saving to a new file using a mock DataFrame instance."""
    video_data = [{'video_id': 'v1', 'title': 'Test Video'}]

    # Patch the DataFrame constructor
    with patch('pandas.DataFrame') as mock_df_class:
        mock_df_instance = MagicMock()
        mock_df_class.return_value = mock_df_instance

        # Call the function under test
        save_progress(video_data, 'Test Species', 'Test Country', 'mock_results.csv')

        # Ensure directories were created
        mock_makedirs.assert_called_once()

        # Ensure DataFrame was instantiated with the correct data
        mock_df_class.assert_called_once_with(video_data)

        # Ensure to_csv was called
        mock_df_instance.to_csv.assert_called_once()

        # Optional: verify the instance type
        saved_df = mock_df_class.return_value
        assert isinstance(saved_df, MagicMock)

# --- Test the main function logic ---
@patch('src.activity_mining.get_youtube_mentions_final.update_progress')
@patch('src.activity_mining.get_youtube_mentions_final.save_progress')
@patch('src.activity_mining.get_youtube_mentions_final.fetch_videos')
@patch('src.activity_mining.get_youtube_mentions_final.load_progress')
def test_get_video_data_main_flow(mock_load_progress, mock_fetch_videos, mock_save_progress, mock_update_progress, mock_file_io, mock_youtube_client):
    """Test the main function's logic flow."""
    mock_load_progress.return_value = pd.DataFrame(columns=['species', 'country', 'timestamp'])
    mock_fetch_videos.return_value = ([{'video_id': 'vid_test', 'species': 'Species A', 'country': 'Croatia'}], 1)
    
    species_list = ['Species A']
    countries = [{"country": "Croatia", "location": "1,1", "radius": "100km"}]
    
    # Pass the mock_youtube_client fixture to the function under test
    get_video_data_for_species_and_countries(mock_youtube_client, species_list, countries, "2024-01-01", "2024-02-01", 'mock_progress.csv')

    assert mock_fetch_videos.call_count == 1
    assert mock_save_progress.call_count == 1
    assert mock_update_progress.call_count == 1
    
# Test the skipping logic
@patch('src.activity_mining.get_youtube_mentions_final.update_progress')
@patch('src.activity_mining.get_youtube_mentions_final.save_progress')
@patch('src.activity_mining.get_youtube_mentions_final.fetch_videos')
@patch('src.activity_mining.get_youtube_mentions_final.load_progress')
def test_get_video_data_skipping(mock_load_progress, mock_fetch_videos, mock_save_progress, mock_update_progress, mock_file_io, mock_youtube_client):
    """Test that the main function skips already processed entries."""
    mock_load_progress.return_value = pd.DataFrame({
        'species': ['Species A'],
        'country': ['Croatia'],
        'timestamp': ['2024-01-01 10:00:00']
    })

    species_list = ['Species A']
    countries = [{"country": "Croatia", "location": "1,1", "radius": "100km"}]
    
    # Pass the mock_youtube_client fixture to the function under test
    get_video_data_for_species_and_countries(mock_youtube_client, species_list, countries, "2024-01-01", "2024-02-01", 'mock_progress.csv')

    assert mock_fetch_videos.call_count == 0
    assert mock_save_progress.call_count == 0
    assert mock_update_progress.call_count == 0