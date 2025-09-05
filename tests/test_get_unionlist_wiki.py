# tests/test_get_unionlist_wiki.py

import pytest
import pandas as pd
import requests
import requests_mock
from bs4 import BeautifulSoup
from src.list_mining.get_unionlist_wiki import (
    extract_scientific_names,
    get_wikidata_q_number,
    fetch_sitelinks,
    EU_LANGUAGES
)

# Mock HTML content for testing `extract_scientific_names`
MOCK_WIKI_HTML = """
<html>
<body>
    <table class="wikitable sortable css-serial">
        <thead>
            <tr>
                <th>No.</th>
                <th>Scientific name</th>
                <th>Common name</th>
            </tr>
        </thead>
        <tbody>
            <tr>
                <td>1</td>
                <td><i>Aedes albopictus</i></td>
                <td>Tiger mosquito</td>
            </tr>
            <tr>
                <td>2</td>
                <td><i>Myocastor coypus</i></td>
                <td>Coypu</td>
            </tr>
            <tr>
                <td>3</td>
                <td><b>Some other species</b></td>
                <td>Non-italic species</td>
            </tr>
            <tr>
                <td>4</td>
                <td></td>
                <td>Empty cell</td>
            </tr>
        </tbody>
    </table>
</body>
</html>
"""

# Mock HTML content for testing `get_wikidata_q_number`
MOCK_SPECIES_WIKI_HTML = """
<html>
<body>
    <div id="p-lang">
        <a href="/wiki/Special:EntityPage/Q596541" title="Wikidata item">Wikidata item</a>
    </div>
</body>
</html>
"""

# Mock JSON content for testing `fetch_sitelinks`
MOCK_WIKIDATA_JSON = {
    "entities": {
        "Q596541": {
            "sitelinks": {
                "enwiki": {"site": "enwiki", "title": "Aedes albopictus"},
                "dewiki": {"site": "dewiki", "title": "Asiatische Tigermücke"},
                "frwiki": {"site": "frwiki", "title": "Moustique tigre"},
                "itwiki": {"site": "itwiki", "title": "Aedes albopictus"}
            }
        }
    }
}

def test_extract_scientific_names_success():
    """Test that scientific names are correctly extracted from mock HTML."""
    expected_names = ['Aedes albopictus', 'Myocastor coypus']
    assert extract_scientific_names(MOCK_WIKI_HTML) == expected_names

def test_extract_scientific_names_no_table():
    """Test the function's behavior when the target table is not found."""
    assert extract_scientific_names("<html><body>No table here.</body></html>") == []

def test_get_wikidata_q_number_success(requests_mock):
    """Test that the Wikidata Q-number is correctly extracted."""
    species_name = "Aedes albopictus"
    mock_url = f'https://en.wikipedia.org/wiki/{species_name.replace(" ", "_")}'
    requests_mock.get(mock_url, text=MOCK_SPECIES_WIKI_HTML)
    
    q_number = get_wikidata_q_number(species_name)
    assert q_number == "Q596541"

def test_get_wikidata_q_number_not_found(requests_mock):
    """Test behavior when the Wikidata link is not found on the page."""
    species_name = "Nonexistent Species"
    mock_url = f'https://en.wikipedia.org/wiki/{species_name.replace(" ", "_")}'
    requests_mock.get(mock_url, text="<html><body>No Wikidata link.</body></html>")
    
    q_number = get_wikidata_q_number(species_name)
    assert q_number is None

def test_get_wikidata_q_number_request_failure(requests_mock, capsys):
    """Test that the function handles request failures gracefully."""
    species_name = "FailureSpecies"
    mock_url = f'https://en.wikipedia.org/wiki/{species_name.replace(" ", "_")}'
    requests_mock.get(mock_url, exc=requests.exceptions.RequestException)
    
    q_number = get_wikidata_q_number(species_name)
    assert q_number is None
    
    # Check that an error message was printed to the console
    captured = capsys.readouterr()
    assert "Error fetching Q-number for FailureSpecies" in captured.out

def test_fetch_sitelinks_success(requests_mock):
    """Test that sitelinks are correctly fetched and filtered."""
    q_number = "Q596541"
    mock_url = f"https://www.wikidata.org/wiki/Special:EntityData/{q_number}.json"
    requests_mock.get(mock_url, json=MOCK_WIKIDATA_JSON)
    
    sitelinks = fetch_sitelinks(q_number)
    
    # Assert that all relevant EU languages are in the result
    assert "en" in sitelinks
    assert "de" in sitelinks
    assert "fr" in sitelinks
    assert "it" in sitelinks
    
    # Assert that the titles are correct
    assert sitelinks["en"] == "Aedes albopictus"
    assert sitelinks["de"] == "Asiatische Tigermücke"
    
    # Assert that a language not in the mock response is still None
    assert sitelinks["es"] is None

def test_fetch_sitelinks_no_entity_data(requests_mock):
    """Test behavior when the Wikidata JSON response has no entity data."""
    q_number = "Q12345"
    mock_url = f"https://www.wikidata.org/wiki/Special:EntityData/{q_number}.json"
    requests_mock.get(mock_url, json={"entities": {}})
    
    sitelinks = fetch_sitelinks(q_number)
    # The function should return a dictionary with all languages but no titles
    assert all(value is None for value in sitelinks.values())
   