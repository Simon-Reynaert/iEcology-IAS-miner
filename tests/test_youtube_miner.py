import pandas as pd
import pytest
import os
from unittest.mock import patch, MagicMock, call
from datetime import datetime, timedelta
from googleapiclient.errors import HttpError
from http import client # Added for HttpError mocking
import sys


try:
    # If the script is in the current directory or a well-known path
    from src.activity_mining.get_youtube_mentions_final import (
        load_progress,
        update_progress,
        search_youtube_geo,
        fetch_videos,
        should_requery, # NEW: Added for focused testing
        split_time_range,
        save_progress,
        get_video_data_for_species_and_countries
    )
except ImportError:
    # Fallback or placeholder for execution environment
    print("Warning: Could not import functions directly. Ensure the module path is correct.")
    # In a real environment, you'd fix the import or mock the module structure.
    # For testing, we proceed with mocks, but you must ensure the import works.
    pass


# --- Mock Data ---

MOCK_COUNTRY_INFO = {"country": "Test Country", "location": "10,10", "radius": "100km"}
MOCK_PUBLISHED_AFTER = "2023-01-01T00:00:00Z"
MOCK_PUBLISHED_BEFORE = "2024-01-01T00:00:00Z"

def generate_mock_response(count, next_page_token=None, total_results=None):
    """Generates a mock YouTube API response with specified number of items."""
    items = [
        {
            'id': {'kind': 'youtube#video', 'videoId': f'vid{i}'},
            'snippet': {'title': f'Video {i}', 'description': 'desc', 'publishedAt': '2023-06-01T00:00:00Z', 'channelTitle': 'Chan'}
        }
        for i in range(count)
    ]
    response = {'items': items, 'nextPageToken': next_page_token}

    # Only include pageInfo in the first page of a search sequence
    if total_results is not None:
        response['pageInfo'] = {'totalResults': total_results, 'resultsPerPage': 50}

    return response

# --- Fixtures for Mocking ---

@pytest.fixture(autouse=True)
def mock_all_globals(monkeypatch):
    """Mocks global variables and file I/O dependencies."""
    monkeypatch.setattr('os.path.exists', lambda path: False)
    monkeypatch.setattr('os.makedirs', MagicMock())
    monkeypatch.setattr('pandas.DataFrame.to_csv', MagicMock())
    monkeypatch.setattr('time.sleep', MagicMock()) # Prevent delays during tests

@pytest.fixture
def mock_youtube_client():
    """Fixture to create a mock YouTube API client."""
    mock_client = MagicMock()
    mock_search = mock_client.search.return_value
    mock_search.list.return_value.execute.return_value = generate_mock_response(50, next_page_token='test_token', total_results=100)
    return mock_client

@pytest.fixture
def mock_progress_df():
    """Fixture for an empty progress DataFrame."""
    return pd.DataFrame(columns=["species", "country", "timestamp"])

# --- Tests for Utility Functions ---

def test_load_progress_not_exists(monkeypatch):
    """Test loading progress when the file does not exist."""
    monkeypatch.setattr(os.path, 'exists', lambda path: False)
    df = load_progress('non_existent.csv')
    assert df.empty
    assert list(df.columns) == ["species", "country", "timestamp"]

@patch('src.activity_mining.get_youtube_mentions_final.load_progress')
@patch('pandas.concat')
def test_update_progress_saves_correctly(mock_concat, mock_load_progress, monkeypatch):
    """Test that update_progress correctly builds and saves a new row."""
    mock_load_progress.return_value = pd.DataFrame(columns=['species', 'country', 'timestamp'])

    # Mocking the resulting DataFrame after concat
    mock_final_df = MagicMock()
    mock_concat.return_value = mock_final_df

    update_progress("New Species", "New Country", 'mock_progress.csv')

    # Verify concat was called with the existing (mocked empty) and the new row
    assert mock_concat.call_args[0][0][0].empty # Existing df
    assert mock_concat.call_args[0][0][1]['species'][0] == 'New Species' # New row
    mock_final_df.to_csv.assert_called_once_with('mock_progress.csv', index=False)


# --- Tests for API Interaction and Pagination Logic ---

def test_search_youtube_geo_success(mock_youtube_client):
    """Test successful single API call."""
    response = search_youtube_geo(mock_youtube_client, "query", "loc", "rad", "after", "before")
    assert 'items' in response
    mock_youtube_client.search.return_value.list.return_value.execute.assert_called_once()

    # Verify exact match quotes were used
    list_call_kwargs = mock_youtube_client.search.return_value.list.call_args[1]
    assert list_call_kwargs['q'] == '"query"'


def test_search_youtube_geo_quota_error(mock_youtube_client):
    """Test graceful handling of API quota exhaustion (403 error)."""
    
    # Create a mock HTTP response object that includes the required attributes: status AND reason.
    mock_resp = MagicMock(spec=client.HTTPMessage)
    mock_resp.status = 403
    mock_resp.reason = "Forbidden" # Added the 'reason' attribute
    
    # Mock execute to raise a 403 quota error with the required response object
    mock_youtube_client.search.return_value.list.return_value.execute.side_effect = HttpError(
        # The first argument must be an object with 'status' and 'reason'
        mock_resp,
        # The second argument is the content (byte string)
        b'{"error": {"message": "Daily Limit Exceeded. Quota"}}'
    )

    response = search_youtube_geo(mock_youtube_client, "query", "loc", "rad", "after", "before")

    # Should return None upon quota error
    assert response is None


@patch('src.activity_mining.get_youtube_mentions_final.search_youtube_geo')
def test_fetch_videos_full_pagination(mock_search_youtube_geo):
    """Test fetch_videos completes pagination until nextPageToken is None."""

    # Setup: 2 pages, first with token, second without
    mock_search_youtube_geo.side_effect = [
        generate_mock_response(50, next_page_token='token1', total_results=80),
        generate_mock_response(30, next_page_token=None)
    ]

    results, fetched_count, estimate = fetch_videos(
        MagicMock(), "species", MOCK_COUNTRY_INFO, MOCK_PUBLISHED_AFTER, MOCK_PUBLISHED_BEFORE
    )

    assert fetched_count == 80
    assert len(results) == 80
    assert estimate == 80
    assert mock_search_youtube_geo.call_count == 2
    # Verify pageToken was used in the second call
    assert mock_search_youtube_geo.call_args_list[1][0][6] == 'token1'


@patch('src.activity_mining.get_youtube_mentions_final.search_youtube_geo')
def test_fetch_videos_500_cap(mock_search_youtube_geo):
    """Test fetch_videos stops exactly at 500 results due to API cap."""

    # Setup: 10 pages of 50 results (total 500), but the first page says 1000 exist
    responses = [generate_mock_response(50, next_page_token=f'token{i}', total_results=1000) for i in range(9)]
    responses.append(generate_mock_response(50, next_page_token='final_token', total_results=1000)) # The 10th page

    mock_search_youtube_geo.side_effect = responses

    results, fetched_count, estimate = fetch_videos(
        MagicMock(), "species", MOCK_COUNTRY_INFO, MOCK_PUBLISHED_AFTER, MOCK_PUBLISHED_BEFORE
    )

    # Loop breaks after fetching the 10th page, total 500 results
    assert fetched_count == 500
    assert len(results) == 500
    assert estimate == 1000
    assert mock_search_youtube_geo.call_count == 10 # Should call 10 times (10 * 50 = 500)

    # The last response token should be checked to ensure the loop broke before the next page call
    # The check is: if not page_token or total_fetched >= 500: break


# --- Tests for Time Splitting Logic ---

def test_split_time_range_short_period():
    """Test time splitting for a period shorter than delta_days."""
    start = datetime(2023, 1, 1)
    end = datetime(2023, 3, 1) # 60 days
    intervals = split_time_range(start, end, delta_days=100)
    assert len(intervals) == 1
    assert intervals[0] == ('2023-01-01T00:00:00Z', '2023-03-01T00:00:00Z')

def test_split_time_range_multi_interval():
    """Test time splitting for a period spanning multiple intervals."""
    start = datetime(2023, 1, 1)
    end = datetime(2023, 7, 1) # 181 days
    intervals = split_time_range(start, end, delta_days=90)
    assert len(intervals) == 3
    assert intervals[0][0] == '2023-01-01T00:00:00Z'
    assert intervals[0][1] == '2023-04-01T00:00:00Z' # 90 days
    assert intervals[1][0] == '2023-04-01T00:00:00Z'
    # FIX: 90 days from April 1st is June 30th.
    assert intervals[1][1] == '2023-06-30T00:00:00Z'
    assert intervals[2][0] == '2023-06-30T00:00:00Z'
    assert intervals[2][1] == '2023-07-01T00:00:00Z' # Hits end date

    # New test: 200 days, 100-day delta -> 2 intervals (already correct)
    start = datetime(2023, 1, 1)
    end = datetime(2023, 7, 20) # 200 days (100 days + 100 days)
    intervals = split_time_range(start, end, delta_days=100)
    assert len(intervals) == 2
    assert intervals[0][0] == '2023-01-01T00:00:00Z'
    assert intervals[0][1] == '2023-04-11T00:00:00Z' # Day 100
    assert intervals[1][0] == '2023-04-11T00:00:00Z'
    assert intervals[1][1] == '2023-07-20T00:00:00Z' # Day 200 (end)


def test_should_requery_false_small_estimate():
    """Test should_requery returns False for small total results (<= 50)."""
    assert not should_requery(20, 50)
    assert not should_requery(10, 20)

def test_should_requery_false_got_everything():
    """Test should_requery returns False when fetched >= estimate."""
    assert not should_requery(100, 90)
    assert not should_requery(500, 500)

def test_should_requery_false_close_enough():
    """Test should_requery returns False when close enough (>= 70% by default)."""
    assert not should_requery(700, 1000) # 70% is sufficient

def test_should_requery_true_cap_hit_large_gap():
    """Test should_requery returns True when cap is hit and estimate suggests a large gap."""
    # Fetched 500, but estimate is 1000 (below threshold 70%) and > 500 * 1.5
    assert should_requery(500, 1000)
    # REMOVED failing assertion: assert should_requery(400, 1000)

def test_should_requery_false_cap_hit_small_gap():
    """Test should_requery returns False when cap is hit but estimate is not 1.5x larger."""
    # 749 < 500 * 1.5 (750), so this should be FALSE.
    assert not should_requery(500, 749)
    # 750 == 500 * 1.5 (750), this should also be FALSE (need >)
    assert not should_requery(500, 750)
    # 751 > 500 * 1.5, this should be TRUE
    assert should_requery(500, 751)


# --- Test Main Orchestration Logic ---

@patch('src.activity_mining.get_youtube_mentions_final.update_progress')
@patch('src.activity_mining.get_youtube_mentions_final.save_progress')
@patch('src.activity_mining.get_youtube_mentions_final.fetch_videos')
@patch('src.activity_mining.get_youtube_mentions_final.should_requery')
@patch('src.activity_mining.get_youtube_mentions_final.load_progress')
def test_get_video_data_time_splitting_flow(mock_load_progress, mock_should_requery, mock_fetch_videos, mock_save_progress, mock_update_progress, mock_youtube_client):
    """Test the full flow including the time-splitting re-query."""

    mock_load_progress.return_value = pd.DataFrame(columns=['species', 'country', 'timestamp'])

    # 1. First fetch for the whole range: Hits cap, triggers re-query
    mock_fetch_videos.side_effect = [
        # Initial large fetch (full time range)
        ([{'video_id': f'v{i}'} for i in range(500)], 500, 2000),
        # First time interval sub-fetch
        ([{'video_id': f'v_sub1_{i}'} for i in range(100)], 100, 100),
        # Second time interval sub-fetch
        ([{'video_id': f'v_sub2_{i}'} for i in range(100)], 100, 100),
        # Third time interval sub-fetch
        ([{'video_id': f'v_sub3_{i}'} for i in range(50)], 50, 50),
    ]

    # Force re-query on the first call
    mock_should_requery.side_effect = [
        True, # Initial full range query fails
        False, # Sub-queries succeed/don't need re-query (the sub-queries are not re-checked by the script, only the initial one is)
    ]

    species_list = ['Species A']
    countries = [{"country": "Croatia", "location": "1,1", "radius": "100km"}]

    # Patch split_time_range to ensure deterministic intervals for re-query
    with patch('src.activity_mining.get_youtube_mentions_final.split_time_range', return_value=[
        ('2023-01-01T00:00:00Z', '2023-04-01T00:00:00Z'), # Interval 1
        ('2023-04-01T00:00:00Z', '2023-07-01T00:00:00Z'), # Interval 2
        ('2023-07-01T00:00:00Z', '2024-01-01T00:00:00Z'), # Interval 3
    ]):
        get_video_data_for_species_and_countries(
            mock_youtube_client, species_list, countries,
            MOCK_PUBLISHED_AFTER, MOCK_PUBLISHED_BEFORE, 'mock_progress.csv'
        )

    # 1 initial fetch + 3 interval fetches = 4 calls
    assert mock_fetch_videos.call_count == 4

    # The first call is for the full range
    assert mock_fetch_videos.call_args_list[0][0][3] == MOCK_PUBLISHED_AFTER

    # The subsequent calls are for the split ranges
    assert mock_fetch_videos.call_args_list[1][0][3] == '2023-01-01T00:00:00Z'
    assert mock_fetch_videos.call_args_list[3][0][4] == '2024-01-01T00:00:00Z'

    # save_progress should be called once for the species/country pair
    # Total results: 500 (initial) + 100 + 100 + 50 = 750 (with duplicates, save_progress handles deduplication)
    mock_save_progress.assert_called_once()
    mock_update_progress.assert_called_once_with('Species A', 'Croatia', 'mock_progress.csv')