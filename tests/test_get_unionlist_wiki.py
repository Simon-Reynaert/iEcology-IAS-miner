"""
Unit tests for the wiki sitelinks pipeline.

Tests the core functions for extracting scientific names, fetching Q-numbers,
and retrieving Wikipedia sitelinks.
"""

import pytest
import pandas as pd
import requests
import requests_mock
from bs4 import BeautifulSoup
from src.list_mining.get_unionlist_wiki import (
    extract_scientific_names,
    get_wikidata_q_number,
    fetch_sitelinks,
    fetch_webpage_content,
    run_wiki_sitelinks_pipeline,
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


class TestExtractScientificNames:
    """Tests for extract_scientific_names function."""
    
    def test_extract_scientific_names_success(self):
        """Test that scientific names are correctly extracted from HTML."""
        expected_names = ['Aedes albopictus', 'Myocastor coypus']
        result = extract_scientific_names(MOCK_WIKI_HTML.encode('utf-8'))
        assert result == expected_names
    
    def test_extract_scientific_names_no_table(self):
        """Test behavior when the target table is not found."""
        html = "<html><body>No table here.</body></html>"
        result = extract_scientific_names(html.encode('utf-8'))
        assert result == []
    
    def test_extract_scientific_names_empty_table(self):
        """Test behavior with an empty table."""
        html = """
        <html>
        <body>
            <table class="wikitable sortable css-serial">
                <thead><tr><th>Header</th></tr></thead>
                <tbody></tbody>
            </table>
        </body>
        </html>
        """
        result = extract_scientific_names(html.encode('utf-8'))
        assert result == []


class TestGetWikidataQNumber:
    """Tests for get_wikidata_q_number function."""
    
    def test_get_wikidata_q_number_success(self, requests_mock):
        """Test that the Wikidata Q-number is correctly extracted."""
        species_name = "Aedes albopictus"
        mock_url = (
            f'https://en.wikipedia.org/wiki/{species_name.replace(" ", "_")}'
        )
        requests_mock.get(mock_url, text=MOCK_SPECIES_WIKI_HTML)
        
        q_number = get_wikidata_q_number(species_name)
        assert q_number == "Q596541"
    
    def test_get_wikidata_q_number_not_found(self, requests_mock):
        """Test behavior when the Wikidata link is not found."""
        species_name = "Nonexistent Species"
        mock_url = (
            f'https://en.wikipedia.org/wiki/{species_name.replace(" ", "_")}'
        )
        requests_mock.get(
            mock_url,
            text="<html><body>No Wikidata link.</body></html>"
        )
        
        q_number = get_wikidata_q_number(species_name)
        assert q_number is None
    
    def test_get_wikidata_q_number_request_failure(self, requests_mock):
        """Test that the function handles request failures gracefully."""
        species_name = "FailureSpecies"
        mock_url = (
            f'https://en.wikipedia.org/wiki/{species_name.replace(" ", "_")}'
        )
        requests_mock.get(mock_url, exc=requests.exceptions.RequestException)
        
        q_number = get_wikidata_q_number(species_name)
        assert q_number is None
    
    def test_get_wikidata_q_number_with_spaces(self, requests_mock):
        """Test handling of species names with multiple spaces."""
        species_name = "Multiple Word Species"
        expected_url = 'https://en.wikipedia.org/wiki/Multiple_Word_Species'
        requests_mock.get(expected_url, text=MOCK_SPECIES_WIKI_HTML)
        
        q_number = get_wikidata_q_number(species_name)
        assert q_number == "Q596541"


class TestFetchSitelinks:
    """Tests for fetch_sitelinks function."""
    
    def test_fetch_sitelinks_success(self, requests_mock):
        """Test that sitelinks are correctly fetched and filtered."""
        q_number = "Q596541"
        mock_url = (
            f"https://www.wikidata.org/wiki/Special:EntityData/"
            f"{q_number}.json"
        )
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
    
    def test_fetch_sitelinks_no_entity_data(self, requests_mock):
        """Test behavior when Wikidata JSON has no entity data."""
        q_number = "Q12345"
        mock_url = (
            f"https://www.wikidata.org/wiki/Special:EntityData/"
            f"{q_number}.json"
        )
        requests_mock.get(mock_url, json={"entities": {}})
        
        sitelinks = fetch_sitelinks(q_number)
        # Should return a dictionary with all languages but no titles
        assert all(value is None for value in sitelinks.values())
    
    def test_fetch_sitelinks_empty_q_number(self):
        """Test behavior with empty or None Q-number."""
        sitelinks_none = fetch_sitelinks(None)
        sitelinks_empty = fetch_sitelinks("")
        
        assert all(value is None for value in sitelinks_none.values())
        assert all(value is None for value in sitelinks_empty.values())
    
    def test_fetch_sitelinks_with_nan(self):
        """Test behavior with pandas NaN Q-number."""
        sitelinks = fetch_sitelinks(pd.NA)
        assert all(value is None for value in sitelinks.values())
    
    def test_fetch_sitelinks_request_failure(self, requests_mock):
        """Test handling of network errors."""
        q_number = "Q123456"
        mock_url = (
            f"https://www.wikidata.org/wiki/Special:EntityData/"
            f"{q_number}.json"
        )
        requests_mock.get(mock_url, exc=requests.exceptions.RequestException)
        
        sitelinks = fetch_sitelinks(q_number)
        assert all(value is None for value in sitelinks.values())
    
    def test_fetch_sitelinks_filters_non_eu_languages(self, requests_mock):
        """Test that non-EU languages are filtered out."""
        q_number = "Q123"
        mock_data = {
            "entities": {
                "Q123": {
                    "sitelinks": {
                        "enwiki": {"site": "enwiki", "title": "Test"},
                        "zhwiki": {"site": "zhwiki", "title": "测试"},
                        "jawiki": {"site": "jawiki", "title": "テスト"}
                    }
                }
            }
        }
        mock_url = (
            f"https://www.wikidata.org/wiki/Special:EntityData/"
            f"{q_number}.json"
        )
        requests_mock.get(mock_url, json=mock_data)
        
        sitelinks = fetch_sitelinks(q_number)
        
        # English should be present
        assert sitelinks["en"] == "Test"
        # Chinese and Japanese should not be in the keys
        assert "zh" not in sitelinks
        assert "ja" not in sitelinks


class TestFetchWebpageContent:
    """Tests for fetch_webpage_content function."""
    
    def test_fetch_webpage_content_success(self, requests_mock):
        """Test successful webpage fetch."""
        url = "https://example.com/test"
        mock_content = "<html><body>Test content</body></html>"
        requests_mock.get(url, text=mock_content)
        
        response = fetch_webpage_content(url)
        assert response is not None
        assert response.text == mock_content
    
    def test_fetch_webpage_content_failure(self, requests_mock, capsys):
        """Test handling of failed webpage fetch."""
        url = "https://example.com/fail"
        requests_mock.get(url, exc=requests.exceptions.RequestException)
        
        response = fetch_webpage_content(url)
        assert response is None
        
        captured = capsys.readouterr()
        assert "Error fetching URL" in captured.out


class TestRunWikiSitelinksPipeline:
    """Tests for the main pipeline function."""
    
    def test_pipeline_with_failed_fetch(
        self,
        requests_mock,
        tmp_path,
        capsys
    ):
        """Test pipeline behavior when webpage fetch fails."""
        url = "https://example.com/test"
        requests_mock.get(url, exc=requests.exceptions.RequestException)
        
        q_file = tmp_path / "test_q.csv"
        s_file = tmp_path / "test_s.csv"
        
        df_q, df_s = run_wiki_sitelinks_pipeline(
            wiki_url=url,
            q_number_file=str(q_file),
            sitelinks_file=str(s_file)
        )
        
        # Should return empty DataFrames
        assert df_q.empty
        assert df_s.empty
        
        captured = capsys.readouterr()
        assert "Failed to fetch webpage content" in captured.out
    
    def test_pipeline_with_no_species(
        self,
        requests_mock,
        tmp_path,
        capsys
    ):
        """Test pipeline when no species are found in the table."""
        url = "https://example.com/test"
        requests_mock.get(
            url,
            text="<html><body>No table here</body></html>"
        )
        
        q_file = tmp_path / "test_q.csv"
        s_file = tmp_path / "test_s.csv"
        
        df_q, df_s = run_wiki_sitelinks_pipeline(
            wiki_url=url,
            q_number_file=str(q_file),
            sitelinks_file=str(s_file)
        )
        
        # Should return empty DataFrames
        assert df_q.empty
        assert df_s.empty
        
        captured = capsys.readouterr()
        assert "No scientific names found" in captured.out
    
    def test_pipeline_end_to_end(self, requests_mock, tmp_path):
        """Test complete pipeline execution with mocked data."""
        # Mock the initial Wikipedia page
        wiki_url = "https://example.com/species_list"
        requests_mock.get(wiki_url, text=MOCK_WIKI_HTML)
        
        # Mock the species Wikipedia pages
        species_url = 'https://en.wikipedia.org/wiki/Aedes_albopictus'
        requests_mock.get(species_url, text=MOCK_SPECIES_WIKI_HTML)
        
        species_url2 = 'https://en.wikipedia.org/wiki/Myocastor_coypus'
        requests_mock.get(
            species_url2,
            text="<html><body>No Wikidata</body></html>"
        )
        
        # Mock the Wikidata API
        wikidata_url = (
            "https://www.wikidata.org/wiki/Special:EntityData/"
            "Q596541.json"
        )
        requests_mock.get(wikidata_url, json=MOCK_WIKIDATA_JSON)
        
        # Create temporary output files
        q_file = tmp_path / "test_q.csv"
        s_file = tmp_path / "test_s.csv"
        
        # Run pipeline
        df_q, df_s = run_wiki_sitelinks_pipeline(
            wiki_url=wiki_url,
            q_number_file=str(q_file),
            sitelinks_file=str(s_file)
        )
        
        # Check Q-numbers DataFrame
        assert len(df_q) == 2
        assert "Aedes albopictus" in df_q["Scientific Name"].values
        assert "Myocastor coypus" in df_q["Scientific Name"].values
        assert df_q.loc[
            df_q["Scientific Name"] == "Aedes albopictus",
            "Wikidata Q-number"
        ].values[0] == "Q596541"
        
        # Check sitelinks DataFrame
        assert len(df_s) > 0
        assert "Aedes albopictus" in df_s["Scientific Name"].values
        assert "en" in df_s["Language"].values
        
        # Check files were created
        assert q_file.exists()
        assert s_file.exists()
        
        # Verify file contents
        df_q_loaded = pd.read_csv(q_file)
        df_s_loaded = pd.read_csv(s_file)
        
        assert len(df_q_loaded) == len(df_q)
        assert len(df_s_loaded) == len(df_s)


class TestEULanguages:
    """Tests for EU_LANGUAGES constant."""
    
    def test_eu_languages_not_empty(self):
        """Test that EU_LANGUAGES list is not empty."""
        assert len(EU_LANGUAGES) > 0
    
    def test_eu_languages_contains_major_languages(self):
        """Test that major EU languages are included."""
        major_languages = ['en', 'fr', 'de', 'es', 'it', 'nl']
        for lang in major_languages:
            assert lang in EU_LANGUAGES
    
    def test_eu_languages_are_strings(self):
        """Test that all language codes are strings."""
        assert all(isinstance(lang, str) for lang in EU_LANGUAGES)
    
    def test_eu_languages_are_lowercase(self):
        """Test that all language codes are lowercase."""
        assert all(lang.islower() for lang in EU_LANGUAGES)