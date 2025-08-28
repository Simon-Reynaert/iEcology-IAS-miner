# tests/test_GBIF_miner.py

import pytest
from unittest.mock import patch
import pandas as pd
from datetime import datetime, timedelta

# Import the functions from your main script
from src.activity_mining.get_GBIF_observations_final import (
    load_species_list,
    fetch_records_from_gbif,
    save_data_to_csv,
    main
)

@pytest.fixture
def mock_records():
    return [{
        "gbifID": 1,
        "scientificName": "Test Species",
        "decimalLatitude": 10.0,
        "decimalLongitude": 20.0,
        "eventDate": "2020-01-01",
        "basisOfRecord": "HUMAN_OBSERVATION",
        "establishmentMeans": "NATIVE"
    }]


@patch('pygbif.occurrences.search')
def test_fetch_records_from_gbif_success(mock_search, mock_records):
    """Test fetching data successfully with multiple pages."""
    mock_search.side_effect = [
        {'count': 2, 'results': [mock_records[0]]},
        {'count': 2, 'results': [mock_records[0]]},
        {'count': 2, 'results': []},  # Final call to stop the loop
    ]

    records = fetch_records_from_gbif("Test Species", "US", "2020-01-01", "2020-01-31", False)
    assert len(records) == 2
    assert records[0]['scientificName'] == 'Test Species'
    assert mock_search.call_count > 1


@patch('pygbif.occurrences.search')
def test_fetch_records_from_gbif_no_records(mock_search):
    """Test the case where no records are found."""
    mock_search.return_value = {'count': 0, 'results': []}
    records = fetch_records_from_gbif("NonExistent Species", "US", "2020-01-01", "2020-01-31", False)
    assert len(records) == 0
    mock_search.assert_called_once()


@patch('pygbif.occurrences.search', side_effect=Exception("API Error"))
def test_fetch_records_from_gbif_retry_failure(mock_search):
    """Test that the function handles and gives up after retries fail."""
    records = fetch_records_from_gbif("Test Species", "US", "2020-01-01", "2020-01-31", False)
    assert len(records) == 0
    assert mock_search.call_count == 3  # 1 initial + 2 retries


@patch('pygbif.occurrences.search')
def test_fetch_records_from_gbif_with_filtering(mock_search):
    """Test filtering out cultivated records."""
    mock_records_with_cultivated = [
        {"establishmentMeans": "NATIVE", "scientificName": "Test Species"},
        {"establishmentMeans": "CULTIVATED", "scientificName": "Test Species"},
        {"establishmentMeans": "WILD", "scientificName": "Test Species"},
    ]

    mock_search.side_effect = [
        {'count': 3, 'results': mock_records_with_cultivated},
        {'count': 3, 'results': []},  # Empty response to stop loop
    ]

    records = fetch_records_from_gbif("Test Species", "US", "2020-01-01", "2020-01-31", filter_wild=True)
    assert len(records) == 2


@patch('os.path.exists', return_value=True)
@patch('pandas.read_csv', return_value=pd.DataFrame({'Scientific Name': ['SpeciesA', 'SpeciesB']}))
def test_load_species_list_success(mock_read_csv, mock_exists):
    """Test loading a species list from a file."""
    species = load_species_list("fake_path.csv")
    assert species == ["SpeciesA", "SpeciesB"]


@patch('os.path.exists', return_value=False)
def test_load_species_list_file_not_found(mock_exists):
    """Test handling of a missing species file."""
    with pytest.raises(FileNotFoundError):
        load_species_list("nonexistent.csv")


@patch('os.path.exists', return_value=False)
@patch('pandas.DataFrame.to_csv')
def test_save_data_to_csv(mock_to_csv, mock_exists):
    """Test saving data to a CSV file."""
    data_to_save = [{'species': 'A', 'country': 'B'}]
    save_data_to_csv(data_to_save, "test_output.csv", False)
    mock_to_csv.assert_called_once_with("test_output.csv", mode='a', index=False, header=True)


@patch('os.path.exists', return_value=True)
@patch('pandas.DataFrame.to_csv')
def test_save_data_to_csv_no_header(mock_to_csv, mock_exists):
    """Test saving data to a CSV without a header if file already exists."""
    data_to_save = [{'species': 'A', 'country': 'B'}]
    save_data_to_csv(data_to_save, "test_output.csv", True)
    mock_to_csv.assert_called_once_with("test_output.csv", mode='a', index=False, header=False)

def test_large_dataset_logic_with_pagination(mock_records):
    """Test main() splits large datasets by month and handles multiple pages per month (>10k scenario)."""
    from src.activity_mining.get_GBIF_observations_final import main

    with patch('src.activity_mining.get_GBIF_observations_final.load_species_list', return_value=['Test Species']), \
         patch('src.activity_mining.get_GBIF_observations_final.european_countries', ['US']), \
         patch('src.activity_mining.get_GBIF_observations_final.save_data_to_csv') as mock_save_data, \
         patch('pygbif.occurrences.search') as mock_search, \
         patch('os.path.exists', return_value=False), \
         patch('pandas.Timestamp.today', return_value=pd.Timestamp("2016-12-31")):

        # Initial call inside main() to check total records
        initial_call = {'count': 10001, 'results': []}  # triggers monthly splitting

        # Prepare monthly responses: 12 months, each month has 2 pages
        monthly_calls = []
        for _ in range(12):
            # Page 1
            monthly_calls.append({'count': 600, 'results': [mock_records[0]] * 300})
            # Page 2
            monthly_calls.append({'count': 600, 'results': [mock_records[0]] * 300})
            # Page 3 (empty) to stop pagination
            monthly_calls.append({'count': 600, 'results': []})

        mock_search.side_effect = [initial_call] + monthly_calls

        # Run main()
        main()

        # Assertions
        expected_search_calls = 1 + len(monthly_calls)  # initial + all pages
        assert mock_search.call_count == expected_search_calls, \
            f"Expected {expected_search_calls} search calls, got {mock_search.call_count}"

        # save_data_to_csv is called once per month (pages aggregated first)
        expected_save_calls = 12
        assert mock_save_data.call_count == expected_save_calls, \
            f"Expected {expected_save_calls} save calls, got {mock_save_data.call_count}"