import os
import shutil
import pytest
import pandas as pd
from unittest.mock import patch, Mock
from src.activity_mining.get_inaturalist_casual_observations_final import (
    fetch_new_observations,
    write_observations_to_csv,
    main,
    OUTPUT_FOLDER
)

TEST_FOLDER = "test_species_output"
TEST_OUTPUT_FOLDER = os.path.join(TEST_FOLDER, "observations")
os.makedirs(TEST_OUTPUT_FOLDER, exist_ok=True)

@pytest.fixture
def cleanup():
    """Clean up CSV files before and after tests."""
    yield
    if os.path.exists(TEST_OUTPUT_FOLDER):
        shutil.rmtree(TEST_OUTPUT_FOLDER)
    os.makedirs(TEST_OUTPUT_FOLDER, exist_ok=True)

def mock_requests_get_paging(url, params):
    page = int(params.get("page", 1))
    per_page = int(params.get("per_page", 200))
    taxon_name = params.get("taxon_name", "Unknown")

    mock_resp = Mock()
    mock_resp.status_code = 200

    # Rate limit: simulate hitting limit on page 2
    if page == 2:
        mock_resp.headers = {"X-RateLimit-Remaining": "0", "X-RateLimit-Reset": "0"}
    else:
        mock_resp.headers = {"X-RateLimit-Remaining": "10", "X-RateLimit-Reset": "0"}

    # Prepare results
    all_obs = [
        {"id": 1, "species_guess": taxon_name, "observed_on": "2025-01-01",
         "place_guess": "Place1", "geojson": {"coordinates": [10, 20]}, "quality_grade":"casual"},
        {"id": 2, "species_guess": taxon_name, "observed_on": "2025-01-02",
         "place_guess": "Place1", "geojson": {"coordinates": [11, 21]}, "quality_grade":"casual"},
        {"id": 3, "species_guess": taxon_name, "observed_on": "2025-01-03",
         "place_guess": "Place1", "geojson": {"coordinates": [12, 22]}, "quality_grade":"casual"}
    ]

    # Simulate pagination
    start = (page - 1) * per_page
    end = start + per_page
    mock_resp.json.return_value = {"results": all_obs[start:end], "total_results": len(all_obs)}

    return mock_resp

@patch("src.activity_mining.get_inaturalist_casual_observations_final.requests.get", side_effect=mock_requests_get_paging)
def test_fetch_new_observations_pages_and_rate_limit(mock_get, cleanup):
    """Test fetch_new_observations accumulates multiple pages and handles rate limits."""
    species_name = "Species A"
    year = 2025
    place_id = 1

    observations = fetch_new_observations(species_name, year, place_id, per_page=2, output_folder=TEST_OUTPUT_FOLDER)
    
    # All 3 observations should be collected
    assert len(observations) == 3
    ids = [obs["id"] for obs in observations]
    assert ids == [1, 2, 3]
    # Species name in all observations should match
    assert all(obs["species_guess"] == species_name for obs in observations)

@patch("src.activity_mining.get_inaturalist_casual_observations_final.requests.get", side_effect=mock_requests_get_paging)
def test_write_observations_to_csv_and_content(mock_get, cleanup):
    """Test writing observations to CSV and verify content."""
    species_name = "Species A"
    year = 2025
    place_id = 1

    observations = fetch_new_observations(species_name, year, place_id, per_page=2, output_folder=TEST_OUTPUT_FOLDER)
    write_observations_to_csv(species_name, observations, output_folder=TEST_OUTPUT_FOLDER)

    csv_path = os.path.join(TEST_OUTPUT_FOLDER, f"{species_name.replace(' ', '_')}_observations.csv")
    assert os.path.exists(csv_path)

    df = pd.read_csv(csv_path)
    assert df.shape[0] == 3
    assert list(df.columns) == ['id', 'species_guess', 'observed_on', 'place_guess', 'latitude', 'longitude', 'quality']
    assert df['id'].tolist() == [1, 2, 3]
    assert all(df['species_guess'] == species_name)

@patch("src.activity_mining.get_inaturalist_casual_observations_final.requests.get", side_effect=mock_requests_get_paging)
def test_main_creates_csvs_for_multiple_species(mock_get, cleanup):
    """Test main function fetches multiple species, writes CSVs correctly, and uses paging/rate limit."""
    species_csv_path = os.path.join(TEST_FOLDER, "test_species.csv")
    df_species = pd.DataFrame({"Scientific Name": ["Species A", "Species B"]})
    df_species.to_csv(species_csv_path, index=False)

    main(species_csv_path, place_id=1, start_year=2025, output_folder=TEST_OUTPUT_FOLDER)

    # Verify CSVs exist and contain 3 observations per species
    for species in ["Species A", "Species B"]:
        csv_path = os.path.join(TEST_OUTPUT_FOLDER, f"{species.replace(' ', '_')}_observations.csv")
        assert os.path.exists(csv_path)
        df = pd.read_csv(csv_path)
        assert df.shape[0] == 3
        # All rows should have the correct species name
        assert all(df['species_guess'] == species)
