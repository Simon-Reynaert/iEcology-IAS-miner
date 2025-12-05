# tests/test_easin_presence_miner.py

import pytest
from unittest.mock import patch, MagicMock
import pandas as pd
from pathlib import Path

from src.EASIN_mining_and_map_generation.get_unionlist_presence_EASIN_final import fetch_easin_presence


@pytest.fixture
def sample_input_csv(tmp_path):
    """Create a temporary CSV with sample species."""
    df = pd.DataFrame({
        "Scientific Name": ["Species A", "Species B", "Species C"]
    })
    file = tmp_path / "input.csv"
    df.to_csv(file, index=False)
    return file


@pytest.fixture
def mock_easin_response():
    """Mock response from EASIN API."""
    return [
        {
            "Name": "Species A",
            "EUConcernName": "Species A",
            "EASINID": "EASIN123",
            "PresentInCountries": [{"Country": "FR"}, {"Country": "DE"}],
            "Synonyms": [{"Synonym": "SpecA"}]
        },
        {
            "Name": "Species X",
            "EUConcernName": "Species C",
            "EASINID": "EASIN456",
            "PresentInCountries": [{"Country": "IT"}],
            "Synonyms": []
        }
    ]


@patch("src.EASIN_mining_and_map_generation.get_unionlist_presence_EASIN_final.requests.get")
def test_fetch_easin_presence(mock_get, sample_input_csv, mock_easin_response, tmp_path):
    """Test fetch_easin_presence with multiple species and synonyms."""
    
    # Mock API response
    mock_response = MagicMock()
    mock_response.json.return_value = mock_easin_response
    mock_response.raise_for_status = MagicMock()
    mock_get.return_value = mock_response

    output_file = tmp_path / "output.csv"

    rows, missing = fetch_easin_presence(str(sample_input_csv), str(output_file))

    # Check the CSV was created
    assert Path(output_file).exists()

    # Convert rows to DataFrame for easier assertions
    df = pd.DataFrame(rows)

    # All species should appear in the output
    assert set(df["scientific_name"]) == {"Species A", "Species B", "Species C"}

    # Check that all countries from API are present
    assert set(df["country"]) == {"DE", "FR", "IT"}

    # Check presence logic for Species A
    species_a_rows = df[df["scientific_name"] == "Species A"]
    assert len(species_a_rows) == 3  # One row per country
    assert species_a_rows[species_a_rows["country"] == "FR"]["present"].values[0] == "yes"
    assert species_a_rows[species_a_rows["country"] == "DE"]["present"].values[0] == "yes"
    assert species_a_rows[species_a_rows["country"] == "IT"]["present"].values[0] == "no"
    assert species_a_rows["easin_id"].values[0] == "EASIN123"

    # Check presence logic for Species C (matched by EUConcernName)
    species_c_rows = df[df["scientific_name"] == "Species C"]
    assert len(species_c_rows) == 3  # One row per country
    assert species_c_rows[species_c_rows["country"] == "IT"]["present"].values[0] == "yes"
    assert species_c_rows[species_c_rows["country"] == "FR"]["present"].values[0] == "no"
    assert species_c_rows[species_c_rows["country"] == "DE"]["present"].values[0] == "no"
    assert species_c_rows["easin_id"].values[0] == "EASIN456"

    # Check Species B (not in API, should have no presence)
    species_b_rows = df[df["scientific_name"] == "Species B"]
    assert len(species_b_rows) == 3  # One row per country
    assert all(species_b_rows["present"] == "no")
    assert pd.isna(species_b_rows["easin_id"].values[0])

    # Species B should be in missing list
    assert missing == ["Species B"]


@patch("src.EASIN_mining_and_map_generation.get_unionlist_presence_EASIN_final.requests.get")
def test_fetch_easin_presence_with_synonyms(mock_get, tmp_path):
    """Test that synonyms are properly matched."""
    
    # Create input with a species that matches via synonym
    df = pd.DataFrame({
        "Scientific Name": ["SpecA"]
    })
    input_file = tmp_path / "input.csv"
    df.to_csv(input_file, index=False)

    # Mock API response with synonym
    mock_response = MagicMock()
    mock_response.json.return_value = [
        {
            "Name": "Species A",
            "EUConcernName": "Species A",
            "EASINID": "EASIN123",
            "PresentInCountries": [{"Country": "FR"}],
            "Synonyms": [{"Synonym": "SpecA"}]
        }
    ]
    mock_response.raise_for_status = MagicMock()
    mock_get.return_value = mock_response

    output_file = tmp_path / "output.csv"

    rows, missing = fetch_easin_presence(str(input_file), str(output_file))

    df_result = pd.DataFrame(rows)
    
    # Check that synonym was matched
    assert "SpecA" in df_result["scientific_name"].values
    assert df_result[df_result["scientific_name"] == "SpecA"]["easin_id"].values[0] == "EASIN123"
    assert df_result[(df_result["scientific_name"] == "SpecA") & (df_result["country"] == "FR")]["present"].values[0] == "yes"
    assert missing == []


@patch("src.EASIN_mining_and_map_generation.get_unionlist_presence_EASIN_final.requests.get")
def test_fetch_easin_presence_partial_match(mock_get, tmp_path):
    """Test that partial matching works when exact match fails."""
    
    # Create input with a species that requires partial matching
    df = pd.DataFrame({
        "Scientific Name": ["Species Alpha Beta"]
    })
    input_file = tmp_path / "input.csv"
    df.to_csv(input_file, index=False)

    # Mock API response with similar name
    mock_response = MagicMock()
    mock_response.json.return_value = [
        {
            "Name": "Species Alpha",
            "EUConcernName": "Species Alpha",
            "EASINID": "EASIN789",
            "PresentInCountries": [{"Country": "ES"}],
            "Synonyms": []
        }
    ]
    mock_response.raise_for_status = MagicMock()
    mock_get.return_value = mock_response

    output_file = tmp_path / "output.csv"

    rows, missing = fetch_easin_presence(str(input_file), str(output_file))

    df_result = pd.DataFrame(rows)
    
    # Check that partial match was successful
    assert "Species Alpha Beta" in df_result["scientific_name"].values
    assert df_result[df_result["scientific_name"] == "Species Alpha Beta"]["easin_id"].values[0] == "EASIN789"
    assert missing == []


@patch("src.EASIN_mining_and_map_generation.get_unionlist_presence_EASIN_final.requests.get")
def test_fetch_easin_presence_api_error(mock_get, sample_input_csv, tmp_path):
    """Test that API errors are properly raised."""
    
    # Mock API error
    mock_get.return_value.raise_for_status.side_effect = Exception("API Error")

    output_file = tmp_path / "output.csv"

    with pytest.raises(Exception, match="API Error"):
        fetch_easin_presence(str(sample_input_csv), str(output_file))


def test_fetch_easin_presence_missing_column(tmp_path):
    """Test that missing 'Scientific Name' column raises an error."""
    
    # Create CSV without required column
    df = pd.DataFrame({
        "Species": ["Species A", "Species B"]
    })
    input_file = tmp_path / "input.csv"
    df.to_csv(input_file, index=False)

    output_file = tmp_path / "output.csv"

    with pytest.raises(KeyError, match="(?i)scientific name"):
        fetch_easin_presence(str(input_file), str(output_file))


@patch("src.EASIN_mining_and_map_generation.get_unionlist_presence_EASIN_final.requests.get")
def test_fetch_easin_presence_empty_countries(mock_get, tmp_path):
    """Test handling of species with no PresentInCountries data."""
    
    df = pd.DataFrame({
        "Scientific Name": ["Species D"]
    })
    input_file = tmp_path / "input.csv"
    df.to_csv(input_file, index=False)

    # Mock API response with empty PresentInCountries
    mock_response = MagicMock()
    mock_response.json.return_value = [
        {
            "Name": "Species D",
            "EUConcernName": "Species D",
            "EASINID": "EASIN999",
            "PresentInCountries": [],
            "Synonyms": []
        }
    ]
    mock_response.raise_for_status = MagicMock()
    mock_get.return_value = mock_response

    output_file = tmp_path / "output.csv"

    rows, missing = fetch_easin_presence(str(input_file), str(output_file))

    # With no countries in the API, there should be no rows generated
    # (since all_countries will be empty)
    assert len(rows) == 0
    assert missing == []


@patch("src.EASIN_mining_and_map_generation.get_unionlist_presence_EASIN_final.requests.get")
def test_fetch_easin_presence_normalization(mock_get, tmp_path):
    """Test that species name normalization (removing authorship) works."""
    
    df = pd.DataFrame({
        "Scientific Name": ["Species E (Author, Year)"]
    })
    input_file = tmp_path / "input.csv"
    df.to_csv(input_file, index=False)

    # Mock API response without authorship
    mock_response = MagicMock()
    mock_response.json.return_value = [
        {
            "Name": "Species E",
            "EUConcernName": "Species E",
            "EASINID": "EASIN111",
            "PresentInCountries": [{"Country": "PT"}],
            "Synonyms": []
        }
    ]
    mock_response.raise_for_status = MagicMock()
    mock_get.return_value = mock_response

    output_file = tmp_path / "output.csv"

    rows, missing = fetch_easin_presence(str(input_file), str(output_file))

    df_result = pd.DataFrame(rows)
    
    # Check that normalization matched the species
    assert "Species E (Author, Year)" in df_result["scientific_name"].values
    assert df_result["easin_id"].values[0] == "EASIN111"
    assert missing == []