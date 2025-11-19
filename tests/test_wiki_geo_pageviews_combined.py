# tests/test_wiki_geo_pageviews_combined.py
import unittest
import pandas as pd
from unittest.mock import patch, MagicMock
from io import StringIO
import tempfile
import os
from datetime import datetime

from src.activity_mining.get_wiki_geo_pageviews_2017_today import (
    fetch_and_process_pageviews,
    get_wikimedia_url,
    URL_HISTORICAL,
    URL_CURRENT,
    DATE_CUTOFF_NEW_DATASET
)


class TestURLSelection(unittest.TestCase):
    """Test the dynamic URL selection based on date"""
    
    def test_historical_url_selection(self):
        """Dates before 2023-02-06 should use historical URL"""
        historical_dates = [
            "2017-02-09",
            "2022-12-31",
            "2023-02-05"
        ]
        for date_str in historical_dates:
            with self.subTest(date=date_str):
                url = get_wikimedia_url(date_str)
                self.assertEqual(url, URL_HISTORICAL)
    
    def test_current_url_selection(self):
        """Dates on or after 2023-02-06 should use current URL"""
        current_dates = [
            "2023-02-06",
            "2023-02-07",
            "2024-01-01",
            "2025-01-01"
        ]
        for date_str in current_dates:
            with self.subTest(date=date_str):
                url = get_wikimedia_url(date_str)
                self.assertEqual(url, URL_CURRENT)


class TestWikiGeoPageviews(unittest.TestCase):
    def setUp(self):
        # Sample species CSV
        self.species_df = pd.DataFrame({
            "Scientific Name": ["Pica pica", "Panthera leo"],
            "Wikidata Q-number": ["Q123", "Q456"]
        })

        # Sample TSV content for two days
        self.tsv_day1 = """Country\tCountry Code\tProject\tWiki Page ID\tArticle Title\tWikidata Q-number\tPageviews
US\tUS\tWikipedia\t1\tPica pica\tQ123\t100
GB\tGB\tWikipedia\t2\tPanthera leo\tQ456\t50
US\tUS\tWikipedia\t3\tPanthera leo\tQ456\t20
FR\tFR\tWikipedia\t4\tPanthera leo\tQ456\t30
"""
        self.tsv_day2 = """Country\tCountry Code\tProject\tWiki Page ID\tArticle Title\tWikidata Q-number\tPageviews
US\tUS\tWikipedia\t1\tPica pica\tQ123\t120
GB\tGB\tWikipedia\t2\tPanthera leo\tQ456\t60
"""
        self.user_agent = "TestUserAgent"

    @patch("requests.get")
    def test_fetch_and_process_current_dates(self, mock_requests_get):
        """Test fetching data for dates using current dataset (>= 2023-02-06)"""
        # Mock requests.get to return TSV content
        mock_resp1 = MagicMock()
        mock_resp1.raise_for_status.return_value = None
        mock_resp1.text = self.tsv_day1

        mock_resp2 = MagicMock()
        mock_resp2.raise_for_status.return_value = None
        mock_resp2.text = self.tsv_day2

        mock_requests_get.side_effect = [mock_resp1, mock_resp2]

        with tempfile.TemporaryDirectory() as tmpdir:
            output_file = os.path.join(tmpdir, "output.csv")
            input_file = os.path.join(tmpdir, "species.csv")

            # Create species CSV file
            self.species_df.to_csv(input_file, index=False)
            
            # Run function with dates in current dataset range
            fetch_and_process_pageviews(
                input_file=input_file,
                output_file=output_file,
                start_date_str="2025-01-01",
                end_date_str="2025-01-02",
                user_agent=self.user_agent
            )
            
            # Verify correct URLs were called
            calls = mock_requests_get.call_args_list
            self.assertEqual(len(calls), 2)
            self.assertIn(URL_CURRENT, calls[0][0][0])
            self.assertIn(URL_CURRENT, calls[1][0][0])
            
            # Read and verify output
            df = pd.read_csv(output_file)
            self._verify_dataframe_structure(df)
            self._verify_dataframe_values(df)

    @patch("requests.get")
    def test_fetch_and_process_historical_dates(self, mock_requests_get):
        """Test fetching data for dates using historical dataset (< 2023-02-06)"""
        mock_resp1 = MagicMock()
        mock_resp1.raise_for_status.return_value = None
        mock_resp1.text = self.tsv_day1

        mock_resp2 = MagicMock()
        mock_resp2.raise_for_status.return_value = None
        mock_resp2.text = self.tsv_day2

        mock_requests_get.side_effect = [mock_resp1, mock_resp2]

        with tempfile.TemporaryDirectory() as tmpdir:
            output_file = os.path.join(tmpdir, "output.csv")
            input_file = os.path.join(tmpdir, "species.csv")

            self.species_df.to_csv(input_file, index=False)
            
            # Run function with dates in historical dataset range
            fetch_and_process_pageviews(
                input_file=input_file,
                output_file=output_file,
                start_date_str="2022-01-01",
                end_date_str="2022-01-02",
                user_agent=self.user_agent
            )
            
            # Verify correct URLs were called
            calls = mock_requests_get.call_args_list
            self.assertEqual(len(calls), 2)
            self.assertIn(URL_HISTORICAL, calls[0][0][0])
            self.assertIn(URL_HISTORICAL, calls[1][0][0])

    @patch("requests.get")
    def test_fetch_and_process_mixed_date_range(self, mock_requests_get):
        """Test fetching data across the cutoff date (using both datasets)"""
        # Need 3 responses: one historical, two current
        mock_resp1 = MagicMock()
        mock_resp1.raise_for_status.return_value = None
        mock_resp1.text = self.tsv_day1

        mock_resp2 = MagicMock()
        mock_resp2.raise_for_status.return_value = None
        mock_resp2.text = self.tsv_day2

        mock_resp3 = MagicMock()
        mock_resp3.raise_for_status.return_value = None
        mock_resp3.text = self.tsv_day1

        mock_requests_get.side_effect = [mock_resp1, mock_resp2, mock_resp3]

        with tempfile.TemporaryDirectory() as tmpdir:
            output_file = os.path.join(tmpdir, "output.csv")
            input_file = os.path.join(tmpdir, "species.csv")

            self.species_df.to_csv(input_file, index=False)
            
            # Run function with date range crossing the cutoff
            fetch_and_process_pageviews(
                input_file=input_file,
                output_file=output_file,
                start_date_str="2023-02-05",
                end_date_str="2023-02-07",
                user_agent=self.user_agent
            )
            
            # Verify both URLs were called appropriately
            calls = mock_requests_get.call_args_list
            self.assertEqual(len(calls), 3)
            # First call should be historical (2023-02-05)
            self.assertIn(URL_HISTORICAL, calls[0][0][0])
            # Second and third calls should be current (2023-02-06, 2023-02-07)
            self.assertIn(URL_CURRENT, calls[1][0][0])
            self.assertIn(URL_CURRENT, calls[2][0][0])

    @patch("requests.get")
    def test_incremental_updates(self, mock_requests_get):
        """Test that existing data is preserved and only new dates are fetched"""
        mock_resp = MagicMock()
        mock_resp.raise_for_status.return_value = None
        mock_resp.text = self.tsv_day2
        mock_requests_get.return_value = mock_resp

        with tempfile.TemporaryDirectory() as tmpdir:
            output_file = os.path.join(tmpdir, "output.csv")
            input_file = os.path.join(tmpdir, "species.csv")

            self.species_df.to_csv(input_file, index=False)
            
            # Create existing output with one date
            existing_df = pd.DataFrame({
                "Scientific Name": ["Pica pica", "Panthera leo"],
                "Country": ["US", "GB"],
                "Wikidata Q-number": ["Q123", "Q456"],
                "2025-01-01": [100, 50]
            })
            existing_df.to_csv(output_file, index=False)
            
            # Fetch only new date
            fetch_and_process_pageviews(
                input_file=input_file,
                output_file=output_file,
                start_date_str="2025-01-01",
                end_date_str="2025-01-02",
                user_agent=self.user_agent
            )
            
            # Should only call API once (for new date)
            self.assertEqual(mock_requests_get.call_count, 1)
            
            # Verify both dates are in output
            df = pd.read_csv(output_file)
            self.assertIn("2025-01-01", df.columns)
            self.assertIn("2025-01-02", df.columns)

    @patch("requests.get")
    def test_error_handling_404(self, mock_requests_get):
        """Test graceful handling of 404 errors"""
        mock_resp = MagicMock()
        mock_resp.raise_for_status.side_effect = Exception("404")
        mock_requests_get.return_value = mock_resp

        with tempfile.TemporaryDirectory() as tmpdir:
            output_file = os.path.join(tmpdir, "output.csv")
            input_file = os.path.join(tmpdir, "species.csv")

            self.species_df.to_csv(input_file, index=False)
            
            # Should not crash on 404
            fetch_and_process_pageviews(
                input_file=input_file,
                output_file=output_file,
                start_date_str="2025-01-01",
                end_date_str="2025-01-01",
                user_agent=self.user_agent
            )

    def test_invalid_date_format(self):
        """Test error handling for invalid date formats"""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_file = os.path.join(tmpdir, "output.csv")
            input_file = os.path.join(tmpdir, "species.csv")

            self.species_df.to_csv(input_file, index=False)
            
            # Should handle invalid date format gracefully
            fetch_and_process_pageviews(
                input_file=input_file,
                output_file=output_file,
                start_date_str="01-01-2025",  # Wrong format
                end_date_str="2025-01-02",
                user_agent=self.user_agent
            )

    def test_missing_species_file(self):
        """Test error handling when species file doesn't exist"""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_file = os.path.join(tmpdir, "output.csv")
            input_file = os.path.join(tmpdir, "nonexistent.csv")
            
            # Should handle missing file gracefully
            fetch_and_process_pageviews(
                input_file=input_file,
                output_file=output_file,
                start_date_str="2025-01-01",
                end_date_str="2025-01-02",
                user_agent=self.user_agent
            )

    def _verify_dataframe_structure(self, df):
        """Helper to verify output dataframe structure"""
        self.assertIsInstance(df, pd.DataFrame)
        
        expected_columns = ["Scientific Name", "Country", "Wikidata Q-number", "2025-01-01", "2025-01-02"]
        self.assertListEqual(sorted(df.columns), sorted(expected_columns))
        
        # Should have 4 unique species-country combinations
        self.assertEqual(len(df), 4)

    def _verify_dataframe_values(self, df):
        """Helper to verify output dataframe values"""
        # Test values for 'Pica pica'
        pica_us_row = df[(df["Scientific Name"] == "Pica pica") & (df["Country"] == "US")]
        self.assertEqual(pica_us_row["2025-01-01"].values[0], 100)
        self.assertEqual(pica_us_row["2025-01-02"].values[0], 120)

        # Test values for 'Panthera leo'
        panthera_us_row = df[(df["Scientific Name"] == "Panthera leo") & (df["Country"] == "US")]
        self.assertEqual(panthera_us_row["2025-01-01"].values[0], 20)
        self.assertEqual(panthera_us_row["2025-01-02"].values[0], 0)

        panthera_gb_row = df[(df["Scientific Name"] == "Panthera leo") & (df["Country"] == "GB")]
        self.assertEqual(panthera_gb_row["2025-01-01"].values[0], 50)
        self.assertEqual(panthera_gb_row["2025-01-02"].values[0], 60)

        panthera_fr_row = df[(df["Scientific Name"] == "Panthera leo") & (df["Country"] == "FR")]
        self.assertEqual(panthera_fr_row["2025-01-01"].values[0], 30)
        self.assertEqual(panthera_fr_row["2025-01-02"].values[0], 0)


if __name__ == "__main__":
    unittest.main()