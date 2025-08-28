import os
import pytest
import pandas as pd
from unittest.mock import patch, Mock
from src.activity_mining.get_inaturalist_casual_observations_final import fetch_new_observations, write_observations_to_csv, OUTPUT_FOLDER

# Ensure the test folder exists
TEST_FOLDER = "test_species_output"
os.makedirs(TEST_FOLDER, exist_ok=True)

# Sample observation data
sample_obs = [
    {"id": 1, "species_guess": "Test species", "observed_on": "2025-01-01", "place_guess": "Test Place",
     "geojson": {"coordinates": [10.0, 20.0]}, "quality_grade": "casual"},
    {"id": 2, "species_guess": "Test species", "observed_on": "2025-01-02", "place_guess": "Test Place",
     "geojson": {"coordinates": [11.0, 21.0]}, "quality_grade": "casual"}
]

@pytest.fixture
def cleanup_csv():
    # Remove test CSVs before and after each test
    yield
    for f in os.listdir(TEST_FOLDER):
        if f.endswith("_observations.csv"):
            os.remove(os.path.join(TEST_FOLDER, f))


@patch("src.activity_mining.get_inaturalist_casual_observations_final.requests.get")
def test_fetch_new_observations(mock_get, cleanup_csv):
    # Mock the API response
    mock_resp = Mock()
    mock_resp.status_code = 200
    mock_resp.headers = {"X-RateLimit-Remaining": "10", "X-RateLimit-Reset": "9999999999"}
    mock_resp.json.return_value = {"results": sample_obs}
    mock_get.return_value = mock_resp

    # First fetch (no existing CSV)
    new_obs = fetch_new_observations("Test species", 2025, 1, per_page=2, output_folder=TEST_FOLDER)
    assert len(new_obs) == 2
    assert all("id" in obs for obs in new_obs)

    # Write to CSV
    write_observations_to_csv("Test species", new_obs, output_folder=TEST_FOLDER)
    csv_path = os.path.join(TEST_FOLDER, "Test_species_observations.csv")
    assert os.path.exists(csv_path)

    # Second fetch: mock API returns same data, should filter out existing IDs
    new_obs_2 = fetch_new_observations("Test species", 2025, 1, per_page=2, output_folder=TEST_FOLDER)
    assert len(new_obs_2) == 0  # already exists in CSV


def test_write_observations_creates_csv(cleanup_csv):
    # Write CSV with a single observation
    write_observations_to_csv("Test species", [sample_obs[0]], output_folder=TEST_FOLDER)
    csv_path = os.path.join(TEST_FOLDER, "Test_species_observations.csv")
    df = pd.read_csv(csv_path)
    assert df.shape[0] == 1
    assert list(df.columns) == ['id', 'species_guess', 'observed_on', 'place_guess', 'latitude', 'longitude', 'quality']
    assert df['id'].iloc[0] == 1
