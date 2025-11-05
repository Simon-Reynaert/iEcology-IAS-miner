import os
import shutil
import pytest
import pandas as pd
from unittest.mock import patch, Mock, mock_open
import time 


# Assuming the import path is correct:
from src.activity_mining.get_inaturalist_nonresearch_observations_final import ( 
    get_taxon_id,
    fetch_observations_by_date_range, 
    run_inat_pipeline,
    write_observations_to_csv,
    BASE_URL,
    TAXA_URL
)

# --- Configuration ---
TEST_FOLDER = "test_species_data"
TEST_OUTPUT_FOLDER = os.path.join(TEST_FOLDER, "observations")

# --- Fixtures ---

@pytest.fixture(scope="module", autouse=True)
def setup_teardown_module():
    """Setup and cleanup for the entire module. Ensure base folder exists."""
    os.makedirs(TEST_FOLDER, exist_ok=True) 
    # NOTE: TEST_OUTPUT_FOLDER is not created here, as it's handled by specific tests/mocks.
    yield
    if os.path.exists(TEST_FOLDER):
        shutil.rmtree(TEST_FOLDER)

@pytest.fixture
def test_csv_path():
    """Creates a temporary species CSV for the pipeline test."""
    path = os.path.join(TEST_FOLDER, "test_species.csv")
    # This explicit creation is fine since TEST_FOLDER is guaranteed by the fixture setup
    df_species = pd.DataFrame({"Scientific Name": ["Species A", "Species B"]})
    df_species.to_csv(path, index=False)
    return path

# --- Mocks ---

def mock_requests_get_combined(url, params, **kwargs):
    # ... (Mock implementation remains the same) ...
    """Mocks both TAXA_URL (ID lookup) and BASE_URL (observations)."""
    mock_resp = Mock()
    mock_resp.status_code = 200

    # 1. Mock TAXA ID lookup
    if url == TAXA_URL:
        q = params.get("q")
        if q == "Species A":
            mock_resp.json.return_value = {"results": [{"id": 1001}]}
        elif q == "Species B":
            mock_resp.json.return_value = {"results": []} 
        else:
            mock_resp.json.return_value = {"results": []}
        return mock_resp

    # 2. Mock Observation Fetching (BASE_URL)
    elif url == BASE_URL:
        page = int(params.get("page", 1))
        per_page = int(params.get("per_page", 200))
        
        taxon_id = params.get("taxon_id")
        taxon_name = params.get("taxon_name")
        
        obs_name = "Species A" if taxon_id == 1001 else taxon_name 

        if page == 2:
            mock_resp.headers = {"X-RateLimit-Remaining": "0", "X-RateLimit-Reset": str(int(time.time()) + 2)} 
        else:
            mock_resp.headers = {"X-RateLimit-Remaining": "10", "X-RateLimit-Reset": "0"}

        all_obs = [
            {"id": 1 + i, "species_guess": obs_name, "observed_on": f"2023-01-0{i+1}",
             "place_guess": "Place1", "geojson": {"coordinates": [10, 20]}, "quality_grade":"casual"}
            for i in range(3)
        ]

        start = (page - 1) * per_page
        end = start + per_page
        mock_resp.json.return_value = {"results": all_obs[start:end], "total_results": len(all_obs)}
        return mock_resp
    
    return Mock(status_code=404)

# --- Tests ---

@patch("src.activity_mining.get_inaturalist_nonresearch_observations_final.requests.get", side_effect=mock_requests_get_combined)
def test_get_taxon_id_success(mock_get):
    """Test successful taxon ID resolution."""
    taxon_id = get_taxon_id("Species A")
    assert taxon_id == 1001
    
@patch("src.activity_mining.get_inaturalist_nonresearch_observations_final.requests.get", side_effect=mock_requests_get_combined)
def test_get_taxon_id_failure(mock_get):
    """Test failed taxon ID resolution."""
    taxon_id = get_taxon_id("Species C")
    assert taxon_id is None

@patch("src.activity_mining.get_inaturalist_nonresearch_observations_final.requests.get", side_effect=mock_requests_get_combined)
@patch("time.sleep", return_value=None) 
def test_fetch_observations_date_range_and_taxon_id(mock_sleep, mock_get):
    """Test fetch_observations_by_date_range uses correct dates, paging, rate limit, and taxon ID."""
    species_name = "Species A" # Will resolve to ID 1001
    place_id = 1
    start_date = "2023-01-01"
    end_date = "2023-12-31"

    observations = fetch_observations_by_date_range(
        species_name, start_date, end_date, place_id, per_page=2, output_folder=TEST_OUTPUT_FOLDER
    )
    
    # All 3 observations should be collected
    assert len(observations) == 3
    
    # Check that the API call used taxon_id, not taxon_name
    # The third request (page=2, which hits rate limit and triggers sleep) will be the most recent one.
    assert mock_get.call_args_list[-1].kwargs['params']['taxon_id'] == 1001
    assert 'taxon_name' not in mock_get.call_args_list[-1].kwargs['params']
    assert mock_sleep.call_count >= 1 # Check rate limit sleep was called

@patch("src.activity_mining.get_inaturalist_nonresearch_observations_final.requests.get", side_effect=mock_requests_get_combined)
@patch("time.sleep", return_value=None)
def test_write_observations_to_csv_and_content(mock_sleep, mock_get): # Retain 'cleanup' for explicit folder setup/teardown
    """Test writing observations to CSV and verify content using the refactored DictWriter."""
    species_name = "Species B"
    place_id = 1
    start_date = "2023-01-01"
    end_date = "2023-12-31"

    # Explicitly ensure the output directory exists for this test, as it performs actual file writes.
    os.makedirs(TEST_OUTPUT_FOLDER, exist_ok=True) 

    observations = fetch_observations_by_date_range(
        species_name, start_date, end_date, place_id, per_page=2, output_folder=TEST_OUTPUT_FOLDER
    )
    write_observations_to_csv(species_name, observations, output_folder=TEST_OUTPUT_FOLDER)

    csv_path = os.path.join(TEST_OUTPUT_FOLDER, f"{species_name.replace(' ', '_')}_observations.csv")
    assert os.path.exists(csv_path)

    df = pd.read_csv(csv_path)
    assert df.shape[0] == 3
    # ... (assertions) ...
    assert df['id'].tolist() == [1, 2, 3]

@patch("src.activity_mining.get_inaturalist_nonresearch_observations_final.requests.get", side_effect=mock_requests_get_combined)
@patch("time.sleep", return_value=None)
@patch("src.activity_mining.get_inaturalist_nonresearch_observations_final.os.makedirs", return_value=None)
@patch("src.activity_mining.get_inaturalist_nonresearch_observations_final.write_observations_to_csv") # <--- PATCHED THE WRITER FUNCTION
def test_run_inat_pipeline_orchestration(mock_write_csv, mock_makedirs, mock_sleep, mock_get, test_csv_path):
    """Test run_inat_pipeline orchestrates fetching and saving for multiple species."""
    start_date = "2023-01-01"
    end_date = "2023-12-31"
    output_folder = "pipeline_test_output"

    q_df, processed_list = run_inat_pipeline(
        test_csv_path,
        place_id=1,
        start_date=start_date,
        end_date=end_date,
        output_folder=output_folder
    )

    # 1. Verify function returns
    assert isinstance(q_df, pd.DataFrame)
    assert processed_list == ["Species A", "Species B"]
    
    # 2. Verify all external API calls were made (Taxon ID check + Paging)
    assert mock_get.call_count >= 4
    
    # 3. Verify that the output writing function was called correctly for both species
    # We check that the list of observations (argument 2) is correct
    assert mock_write_csv.call_count == 2
    
    # Check Species A call (Mock ID 1001, 3 observations)
    species_a_obs = mock_write_csv.call_args_list[0].args[1]
    assert mock_write_csv.call_args_list[0].args[0] == "Species A"
    assert len(species_a_obs) == 3
    
    # Check Species B call (Mock Name, 3 observations)
    species_b_obs = mock_write_csv.call_args_list[1].args[1]
    assert mock_write_csv.call_args_list[1].args[0] == "Species B"
    assert len(species_b_obs) == 3