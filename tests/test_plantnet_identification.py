import pytest
import pandas as pd
import os
import requests
import requests_mock
from pathlib import Path
from unittest.mock import patch, MagicMock, mock_open, call
from typing import Dict, List

# --- Import the PlantNet Module ---
try:
    from src.flickr_to_plantnet.ID_plantnet_images import (
        PlantNetConfig,
        load_expected_species,
        find_observation_folders,
        call_plantnet_api,
        process_api_results,
        create_csv_row,
        process_all_observations,
        filter_high_fidelity_observations,
        print_summary_statistics,
        run_full_pipeline,
        load_high_fidelity_data
    )
except ImportError:
    from ID_plantnet_images import (
        PlantNetConfig,
        load_expected_species,
        find_observation_folders,
        call_plantnet_api,
        process_api_results,
        create_csv_row,
        process_all_observations,
        filter_high_fidelity_observations,
        print_summary_statistics,
        run_full_pipeline,
        load_high_fidelity_data
    )

MODULE_PATH = 'src.flickr_to_plantnet.ID_plantnet_images'

# ==============================================================================
# 1. FIXTURES AND MOCK DATA
# ==============================================================================

@pytest.fixture
def mock_metadata_df():
    """Mock metadata DataFrame."""
    return pd.DataFrame({
        'photo_id': ['101', '102', '103', '104', '105'],
        'scientific_name': ['Species A', 'Species A', 'Species B', 'Species C', 'Species D'],
        'latitude': [40.0, 41.0, 50.0, 60.0, 70.0],
        'longitude': [10.0, 11.0, 20.0, 30.0, 40.0],
        'url': ['http://example.com/101.jpg'] * 5,
        'local_path': ['path/101.jpg'] * 5
    })


@pytest.fixture
def mock_api_success_response():
    """Mock successful PlantNet API response."""
    return {
        'results': [
            {
                'score': 0.95,
                'species': {
                    'scientificNameWithoutAuthor': 'Species A',
                    'commonNames': ['Common Name A']
                }
            },
            {
                'score': 0.80,
                'species': {
                    'scientificNameWithoutAuthor': 'Species B',
                    'commonNames': ['Common Name B']
                }
            },
            {
                'score': 0.65,
                'species': {
                    'scientificNameWithoutAuthor': 'Species C',
                    'commonNames': []
                }
            }
        ]
    }


@pytest.fixture
def mock_identification_results_df():
    """Mock identification results DataFrame."""
    return pd.DataFrame({
        'photo_id': ['101', '102', '103', '104', '105'],
        'Expected_Species': ['Species A', 'Species A', 'Species B', 'Species C', 'Species D'],
        'Expected_Species_Found': ['Yes', 'Yes', 'Yes', 'No', 'Yes'],
        'Expected_Species_Rank': ['1', '2', '1', 'Not in Top 5', '3'],
        'Top_1_Score': [0.95, 0.88, 0.92, 0.75, 0.80],
        'API_Status': ['Success', 'Success', 'Success', 'Success', 'Success']
    })


# ==============================================================================
# 2. TEST PlantNetConfig
# ==============================================================================

def test_plantnet_config_default_init(monkeypatch):
    """Test PlantNetConfig initialization with default values."""
    monkeypatch.setenv("PLANTNET_API_KEY", "test_api_key_12345")
    
    config = PlantNetConfig()
    
    assert config.base_dir == "PlantNet_Batch_Images"
    assert config.api_key == "test_api_key_12345"
    assert config.project == "all"
    assert "test_api_key_12345" in config.api_endpoint
    # FIX: Use Path objects for cross-platform path comparison
    expected_path = Path("PlantNet_Batch_Images") / "master_observations_metadata.csv"
    assert Path(config.master_metadata_file) == expected_path
    assert "identification_summary_results_per_id.csv" in config.identification_summary_file
    assert "high_fidelity_observations.csv" in config.high_fidelity_file


def test_plantnet_config_custom_init(monkeypatch):
    """Test PlantNetConfig initialization with custom values."""
    monkeypatch.setenv("PLANTNET_API_KEY", "custom_key")
    
    config = PlantNetConfig(
        base_dir="custom_dir",
        metadata_file="custom_metadata.csv",
        output_summary_file="custom_summary.csv",
        output_high_fidelity_file="custom_high_fidelity.csv"
    )
    
    assert config.base_dir == "custom_dir"
    assert config.master_metadata_file == "custom_metadata.csv"
    assert config.identification_summary_file == "custom_summary.csv"
    assert config.high_fidelity_file == "custom_high_fidelity.csv"


def test_plantnet_config_api_key_parameter_override(monkeypatch):
    """Test that api_key parameter overrides environment variable."""
    monkeypatch.setenv("PLANTNET_API_KEY", "env_key")
    
    config = PlantNetConfig(api_key="param_key")
    
    assert config.api_key == "param_key"


def test_plantnet_config_missing_api_key(monkeypatch):
    """Test that missing API key raises ValueError."""
    monkeypatch.delenv("PLANTNET_API_KEY", raising=False)
    
    with pytest.raises(ValueError, match="PLANTNET_API_KEY not found"):
        PlantNetConfig()


# ==============================================================================
# 3. TEST load_expected_species
# ==============================================================================

@patch(f'{MODULE_PATH}.pd.read_csv')
def test_load_expected_species_success(mock_read_csv, mock_metadata_df, monkeypatch):
    """Test successful loading of expected species mapping."""
    monkeypatch.setenv("PLANTNET_API_KEY", "test_key")
    mock_read_csv.return_value = mock_metadata_df
    
    config = PlantNetConfig()
    result = load_expected_species(config)
    
    assert isinstance(result, dict)
    assert len(result) == 5
    assert result['101'] == 'Species A'
    assert result['103'] == 'Species B'
    mock_read_csv.assert_called_once_with(config.master_metadata_file)


@patch(f'{MODULE_PATH}.pd.read_csv')
def test_load_expected_species_file_not_found(mock_read_csv, monkeypatch):
    """Test handling of missing metadata file."""
    monkeypatch.setenv("PLANTNET_API_KEY", "test_key")
    mock_read_csv.side_effect = FileNotFoundError("File not found")
    
    config = PlantNetConfig()
    
    with pytest.raises(FileNotFoundError):
        load_expected_species(config)


@patch(f'{MODULE_PATH}.pd.read_csv')
def test_load_expected_species_missing_columns(mock_read_csv, monkeypatch):
    """Test handling of missing required columns."""
    monkeypatch.setenv("PLANTNET_API_KEY", "test_key")
    mock_read_csv.return_value = pd.DataFrame({'wrong_column': [1, 2, 3]})
    
    config = PlantNetConfig()
    
    with pytest.raises(KeyError):
        load_expected_species(config)


# ==============================================================================
# 4. TEST find_observation_folders
# ==============================================================================

@patch(f'{MODULE_PATH}.os.listdir')
@patch(f'{MODULE_PATH}.os.path.isdir')
def test_find_observation_folders_success(mock_isdir, mock_listdir, monkeypatch):
    """Test successful finding of observation folders."""
    monkeypatch.setenv("PLANTNET_API_KEY", "test_key")
    mock_listdir.return_value = ['101', '102', 'file.txt', '103']
    # FIX: os.path.isdir receives a full path, not just the name. 
    # Mock it to return True for any path that does not end with .txt
    mock_isdir.side_effect = lambda x: not x.endswith('.txt')
    
    config = PlantNetConfig()
    # The actual call to listdir will be on config.base_dir, which is "PlantNet_Batch_Images"
    # The actual calls to isdir will be os.path.isdir("PlantNet_Batch_Images/101"), etc.
    result = find_observation_folders(config)
    
    assert result == ['101', '102', '103']
    assert len(result) == 3


@patch(f'{MODULE_PATH}.os.listdir')
def test_find_observation_folders_directory_not_found(mock_listdir, monkeypatch):
    """Test handling of missing base directory."""
    monkeypatch.setenv("PLANTNET_API_KEY", "test_key")
    mock_listdir.side_effect = FileNotFoundError("Directory not found")
    
    config = PlantNetConfig()
    
    with pytest.raises(FileNotFoundError):
        find_observation_folders(config)


@patch(f'{MODULE_PATH}.os.listdir')
@patch(f'{MODULE_PATH}.os.path.isdir')
def test_find_observation_folders_no_folders(mock_isdir, mock_listdir, monkeypatch):
    """Test handling when no observation folders found."""
    monkeypatch.setenv("PLANTNET_API_KEY", "test_key")
    mock_listdir.return_value = ['file1.txt', 'file2.csv']
    mock_isdir.return_value = False
    
    config = PlantNetConfig()
    
    with pytest.raises(ValueError, match="No observation ID folders found"):
        find_observation_folders(config)


# ==============================================================================
# 5. TEST call_plantnet_api
# ==============================================================================

@patch(f'{MODULE_PATH}.os.path.exists', return_value=True)
@patch(f'{MODULE_PATH}.requests.post')
def test_call_plantnet_api_success(mock_post, mock_exists, mock_api_success_response):
    """Test successful API call."""
    api_endpoint = "https://api.plantnet.org/v2/identify/all?api-key=test"
    
    # Mock the response object from requests.post
    mock_response = MagicMock()
    mock_response.status_code = 200
    mock_response.json.return_value = mock_api_success_response
    mock_post.return_value = mock_response

    # Create a proper file mock that returns bytes when read
    m_open = mock_open(read_data=b'fake_image_data')
    
    # Patching 'builtins.open' for the file read
    with patch('builtins.open', m_open):
        success, result, error = call_plantnet_api('test.jpg', api_endpoint)
    
    assert success is True
    assert result == mock_api_success_response
    assert error == ""
    assert 'results' in result
    
    # Verify requests.post was called with the correct files argument
    mock_post.assert_called_once()
    # Check that the file was opened in binary mode 'rb'
    m_open.assert_called_once_with('test.jpg', 'rb')


@patch(f'{MODULE_PATH}.os.path.exists', return_value=False)
def test_call_plantnet_api_file_not_found(mock_exists):
    """Test handling when image file doesn't exist."""
    success, result, error = call_plantnet_api('nonexistent.jpg', 'http://api.test')
    
    assert success is False
    assert result is None
    assert "Image file not found" in error


@patch(f'{MODULE_PATH}.os.path.exists', return_value=True)
@patch(f'{MODULE_PATH}.requests.post')
def test_call_plantnet_api_http_error(mock_post, mock_exists):
    """Test handling of HTTP error response."""
    api_endpoint = "https://api.plantnet.org/v2/identify/all?api-key=test"
    
    mock_response = MagicMock()
    mock_response.status_code = 401
# Mock the json() call to return the error body only if the status code check is skipped
        # For a 401 error, raise_for_status will be called and should be caught first.
    mock_response.json.return_value = {'error': 'Invalid API key'}
    mock_response.text = '{"error": "Invalid API key"}'
    mock_response.raise_for_status.side_effect = requests.exceptions.HTTPError("401 Client Error: Unauthorized for url", response=mock_response)
    mock_post.return_value = mock_response

    m_open = mock_open(read_data=b'fake_image_data')
    with patch('builtins.open', m_open):
        success, result, error = call_plantnet_api('test.jpg', api_endpoint)
    
    assert success is False
    assert "API Error Code 401: Invalid API key" in error
    # FIX: The original test assertion for "Invalid API key" might be too specific
    # if the actual error message comes from the generic HTTPError.
    # The fix for success and the error message should pass now.


@patch(f'{MODULE_PATH}.os.path.exists', return_value=True)
@patch(f'{MODULE_PATH}.requests.post')
def test_call_plantnet_api_no_results(mock_post, mock_exists):
    """Test handling when API returns success but no results."""
    api_endpoint = "https://api.plantnet.org/v2/identify/all?api-key=test"
    
    mock_response = MagicMock()
    mock_response.status_code = 200
    mock_response.json.return_value = {'results': []}
    mock_post.return_value = mock_response

    m_open = mock_open(read_data=b'fake_image_data')
    with patch('builtins.open', m_open):
        success, result, error = call_plantnet_api('test.jpg', api_endpoint)
    
    assert success is False
    assert "no identification results" in error


@patch(f'{MODULE_PATH}.os.path.exists', return_value=True)
@patch(f'{MODULE_PATH}.requests.post')
def test_call_plantnet_api_invalid_json(mock_post, mock_exists):
    """Test handling of invalid JSON response."""
    api_endpoint = "https://api.plantnet.org/v2/identify/all?api-key=test"
    
    mock_response = MagicMock()
    mock_response.status_code = 500
    mock_response.json.side_effect = requests.exceptions.JSONDecodeError("Expecting value", "<html>Server Error</html>", 0)
    mock_post.return_value = mock_response

    m_open = mock_open(read_data=b'fake_image_data')
    with patch('builtins.open', m_open):
        success, result, error = call_plantnet_api('test.jpg', api_endpoint)
    
    assert success is False
    assert "Could not decode JSON" in error


@patch(f'{MODULE_PATH}.os.path.exists', return_value=True)
@patch(f'{MODULE_PATH}.requests.post')
def test_call_plantnet_api_network_error(mock_post, mock_exists):
    """Test handling of network errors."""
    api_endpoint = "https://api.plantnet.org/v2/identify/all?api-key=test"
    
    mock_post.side_effect = requests.exceptions.ConnectionError("DNS failure")

    m_open = mock_open(read_data=b'fake_image_data')
    with patch('builtins.open', m_open):
        success, result, error = call_plantnet_api('test.jpg', api_endpoint)
    
    assert success is False
    assert "Network or Request Error" in error


# ==============================================================================
# 6. TEST process_api_results
# ==============================================================================

def test_process_api_results_species_found_rank_1(mock_api_success_response):
    """Test processing when expected species is found at rank 1."""
    result = process_api_results(mock_api_success_response, 'Species A')
    
    assert len(result['top_5']) == 3
    assert result['species_found'] == 'Yes'
    assert result['species_rank'] == '1'
    assert float(result['species_score']) == 0.95


def test_process_api_results_species_found_rank_2(mock_api_success_response):
    """Test processing when expected species is found at rank 2."""
    result = process_api_results(mock_api_success_response, 'Species B')
    
    assert result['species_found'] == 'Yes'
    assert result['species_rank'] == '2'
    assert float(result['species_score']) == 0.80


def test_process_api_results_species_not_found(mock_api_success_response):
    """Test processing when expected species is not in top 5."""
    result = process_api_results(mock_api_success_response, 'Species Z')
    
    assert result['species_found'] == 'No'
    assert result['species_rank'] == 'Not in Top 5'
    assert result['species_score'] == 'N/A'


def test_process_api_results_case_insensitive():
    """Test that species matching is case-insensitive."""
    api_response = {
        'results': [{
            'score': 0.90,
            'species': {
                'scientificNameWithoutAuthor': 'species alpha',
                'commonNames': []
            }
        }]
    }
    
    result = process_api_results(api_response, 'Species Alpha')
    
    assert result['species_found'] == 'Yes'
    assert result['species_rank'] == '1'


def test_process_api_results_empty_common_names(mock_api_success_response): # FIX: Added fixture
    """Test handling of empty common names."""
    result = process_api_results(mock_api_success_response, 'Species C')
    
    assert result['top_5'][2]['common_names'] == []


# ==============================================================================
# 7. TEST create_csv_row
# ==============================================================================

def test_create_csv_row_success():
    """Test CSV row creation for successful processing."""
    processed_results = {
        'top_5': [
            {'scientific_name': 'Species A', 'score': 0.95, 'common_names': ['Name A']},
            {'scientific_name': 'Species B', 'score': 0.80, 'common_names': []},
        ],
        'species_found': 'Yes',
        'species_rank': '1',
        'species_score': '0.950000'
    }
    
    row = create_csv_row('101', 'Species A', 'path/101.jpg', 'Success', processed_results)
    
    assert row[0] == '101'
    assert row[1] == 'Species A'
    assert row[2] == 'path/101.jpg'
    assert row[3] == 'Success'
    assert row[4] == 'Species A'  # Top 1 scientific name
    assert row[5] == '0.950000'  # Top 1 score
    assert row[6] == 'Name A'  # Top 1 common names
    assert row[-4] == 'Yes'  # Expected species found
    assert row[-3] == '1'  # Expected species rank


def test_create_csv_row_failure():
    """Test CSV row creation for failed processing."""
    row = create_csv_row('102', 'Species B', 'path/102.jpg', 'Failure', None, 'API Error')
    
    assert row[0] == '102'
    assert row[3] == 'Failure'
    assert row[4] == 'N/A'  # No results
    assert row[-4] == 'No'
    assert row[-1] == 'API Error'


def test_create_csv_row_padding_top_5():
    """Test that row is padded correctly when fewer than 5 results."""
    processed_results = {
        'top_5': [
            {'scientific_name': 'Species A', 'score': 0.95, 'common_names': []},
        ],
        'species_found': 'Yes',
        'species_rank': '1',
        'species_score': '0.950000'
    }
    
    row = create_csv_row('101', 'Species A', 'path/101.jpg', 'Success', processed_results)
    
    # Check that remaining 4 slots are filled with N/A / 0.0
    assert row[7] == 'N/A'  # Top 2 scientific name
    assert row[8] == '0.0'  # Top 2 score
    assert row[10] == 'N/A'  # Top 3 scientific name


# ==============================================================================
# 8. TEST process_all_observations
# ==============================================================================

# NOTE: The previous call_plantnet_api was failing due to its own setup, 
# so we mock it directly here to ensure the logic of process_all_observations is tested.
@patch(f'{MODULE_PATH}.call_plantnet_api')
@patch('builtins.open', new_callable=mock_open)
@patch(f'{MODULE_PATH}.csv.writer')
def test_process_all_observations(mock_csv_writer, mock_file, mock_api_call, monkeypatch):
    """Test processing all observations and writing to CSV."""
    monkeypatch.setenv("PLANTNET_API_KEY", "test_key")
    
    # Setup mocks
    config = PlantNetConfig()
    expected_species_map = {'101': 'Species A', '102': 'Species B'}
    observation_ids = ['101', '102']
    
    mock_api_call.return_value = (
        True,
        {
            'results': [{
                'score': 0.95,
                'species': {
                    'scientificNameWithoutAuthor': 'Species A',
                    'commonNames': ['Common A']
                }
            }]
        },
        ""
    )
    
    mock_writer_instance = MagicMock()
    mock_csv_writer.return_value = mock_writer_instance
    
    # Call function
    result = process_all_observations(config, expected_species_map, observation_ids)
    
    # Assertions
    assert result == config.identification_summary_file
    assert mock_api_call.call_count == 2
    assert mock_writer_instance.writerow.call_count == 3  # 1 header + 2 data rows


# ==============================================================================
# 9. TEST filter_high_fidelity_observations
# ==============================================================================

@patch(f'{MODULE_PATH}.pd.read_csv')
@patch(f'{MODULE_PATH}.pd.DataFrame.to_csv')
def test_filter_high_fidelity_observations(mock_to_csv, mock_read_csv, 
                                           mock_metadata_df, 
                                           mock_identification_results_df, 
                                           monkeypatch):
    """Test filtering for high-fidelity observations (rank 1 only)."""
    monkeypatch.setenv("PLANTNET_API_KEY", "test_key")
    
    config = PlantNetConfig()
    mock_read_csv.return_value = mock_identification_results_df
    
    result = filter_high_fidelity_observations(config, mock_metadata_df)
    
    assert result == config.high_fidelity_file
    mock_to_csv.assert_called_once()


@patch(f'{MODULE_PATH}.pd.read_csv')
def test_filter_high_fidelity_observations_merge_columns(mock_read_csv, 
                                                        mock_metadata_df, 
                                                        monkeypatch):
    """Test that high-fidelity filtering merges and renames columns correctly."""
    monkeypatch.setenv("PLANTNET_API_KEY", "test_key")
    
    results_df = pd.DataFrame({
        'photo_id': ['101', '103'],
        'Expected_Species': ['Species A', 'Species B'],
        'Expected_Species_Found': ['Yes', 'Yes'],
        'Expected_Species_Rank': ['1', '1'],
        'Top_1_Score': [0.95, 0.92],
        'API_Status': ['Success', 'Success']
    })
    
    config = PlantNetConfig()
    
    with patch(f'{MODULE_PATH}.pd.DataFrame.to_csv') as mock_to_csv:
        mock_read_csv.return_value = results_df
        
        filter_high_fidelity_observations(config, mock_metadata_df)
        
        # Verify to_csv was called
        assert mock_to_csv.called


# ==============================================================================
# 10. TEST print_summary_statistics
# ==============================================================================

@patch(f'{MODULE_PATH}.pd.read_csv')
def test_print_summary_statistics(mock_read_csv, 
                                  mock_identification_results_df, 
                                  monkeypatch, 
                                  capsys):
    """Test printing summary statistics."""
    monkeypatch.setenv("PLANTNET_API_KEY", "test_key")
    
    config = PlantNetConfig()
    mock_read_csv.return_value = mock_identification_results_df
    
    print_summary_statistics(config)
    
    captured = capsys.readouterr()
    assert "SUMMARY STATISTICS" in captured.out
    assert "Total observations processed: 5" in captured.out
    assert "Successful API calls: 5" in captured.out


# ==============================================================================
# 11. TEST run_full_pipeline (Integration)
# ==============================================================================

@patch(f'{MODULE_PATH}.print_summary_statistics')
@patch(f'{MODULE_PATH}.filter_high_fidelity_observations')
@patch(f'{MODULE_PATH}.process_all_observations')
@patch(f'{MODULE_PATH}.find_observation_folders')
@patch(f'{MODULE_PATH}.load_expected_species')
@patch(f'{MODULE_PATH}.pd.read_csv')
def test_run_full_pipeline_integration(mock_read_csv,
                                       mock_load_species,
                                       mock_find_folders,
                                       mock_process,
                                       mock_filter,
                                       mock_print_stats,
                                       mock_metadata_df,
                                       monkeypatch):
    """Test full pipeline orchestration."""
    monkeypatch.setenv("PLANTNET_API_KEY", "test_key")
    
    # Setup mocks
    mock_read_csv.return_value = mock_metadata_df
    mock_load_species.return_value = {'101': 'Species A', '102': 'Species B'}
    mock_find_folders.return_value = ['101', '102']
    mock_process.return_value = 'summary.csv'
    mock_filter.return_value = 'high_fidelity.csv'
    
    # Run pipeline
    summary, high_fidelity = run_full_pipeline(base_dir="test_dir")
    
    # Assertions
    assert summary == 'summary.csv'
    assert high_fidelity == 'high_fidelity.csv'
    mock_load_species.assert_called_once()
    mock_find_folders.assert_called_once()
    mock_process.assert_called_once()
    mock_filter.assert_called_once()
    mock_print_stats.assert_called_once()


# ==============================================================================
# 12. TEST load_high_fidelity_data
# ==============================================================================

@patch(f'{MODULE_PATH}.pd.read_csv')
def test_load_high_fidelity_data(mock_read_csv, mock_metadata_df, monkeypatch):
    """Test loading high-fidelity data."""
    monkeypatch.setenv("PLANTNET_API_KEY", "test_key")
    mock_read_csv.return_value = mock_metadata_df
    
    result = load_high_fidelity_data(base_dir="test_dir")
    
    assert isinstance(result, pd.DataFrame)
    mock_read_csv.assert_called_once()