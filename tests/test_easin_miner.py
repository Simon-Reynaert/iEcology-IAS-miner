# tests/test_easin_presence.py

import pytest
from unittest.mock import patch
import pandas as pd
from pathlib import Path

from src.activity_mining.get_IAS_presence_EASIN import fetch_easin_presence  # adjust path if needed


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


@patch("src.activity_mining.get_IAS_presence_EASIN.requests.get")
def test_fetch_easin_presence(mock_get, sample_input_csv, mock_easin_response, tmp_path):
    """Test fetch_easin_presence with multiple species and synonyms."""
    
    # Mock API response
    mock_get.return_value.json.return_value = mock_easin_response
    mock_get.return_value.raise_for_status = lambda: None

    output_file = tmp_path / "output.csv"

    rows, missing = fetch_easin_presence(str(sample_input_csv), str(output_file))

    # Check the CSV was created
    assert Path(output_file).exists()

    # Convert rows to DataFrame for easier assertions
    df = pd.DataFrame(rows)

    # All species should appear in the output
    assert set(df["scientific_name"]) == {"Species A", "Species B", "Species C"}

    # Check presence logic
    species_a_rows = df[df["scientific_name"] == "Species A"]
    assert any(species_a_rows["present"] == "yes")
    assert "FR" in species_a_rows[species_a_rows["present"] == "yes"]["country"].values
    assert "DE" in species_a_rows[species_a_rows["present"] == "yes"]["country"].values

    species_c_rows = df[df["scientific_name"] == "Species C"]
    assert any(species_c_rows["present"] == "yes")
    assert species_c_rows[species_c_rows["present"] == "yes"]["country"].values[0] == "IT"

    # Species B should be missing
    assert missing == ["Species B"]
