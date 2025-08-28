# tests/test_wiki_geo_pageviews_historical.py
import unittest
import pandas as pd
from unittest.mock import patch, MagicMock
from io import StringIO
import tempfile
import os

from src.activity_mining.get_wiki_geo_pageviews_historical_final import fetch_and_process_pageviews

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
    def test_fetch_and_process(self, mock_requests_get):
        # We will not mock pandas.read_csv globally.
        # Instead, we will mock pandas.read_csv inside the test,
        # where we can control its behavior precisely.

        # Mock requests.get to return TSV content
        mock_resp1 = MagicMock()
        mock_resp1.raise_for_status.return_value = None
        mock_resp1.text = self.tsv_day1

        mock_resp2 = MagicMock()
        mock_resp2.raise_for_status.return_value = None
        mock_resp2.text = self.tsv_day2

        # Use side_effect to return different responses for each call
        mock_requests_get.side_effect = [mock_resp1, mock_resp2]

        # Temporary file for output CSV
        with tempfile.TemporaryDirectory() as tmpdir:
            output_file = os.path.join(tmpdir, "output.csv")
            species_file = os.path.join(tmpdir, "species.csv")

            # Create a real species CSV file for the function to read
            self.species_df.to_csv(species_file, index=False)
            
            # Use patch.object to mock os.path.exists and avoid file system errors.
            with patch('os.path.exists', return_value=False):
                # Run function
                fetch_and_process_pageviews(
                    species_file=species_file,
                    output_file=output_file,
                    start_date_str="2023-01-01",
                    end_date_str="2023-01-02",
                    user_agent=self.user_agent
                )
            
            # Read the final output CSV normally (no mocking) to check results
            df = pd.read_csv(output_file)

            # --- Assertions ---
            self.assertIsInstance(df, pd.DataFrame)
            
            # The output should be a single dataframe with columns for species, country, and each date
            expected_columns = ["Scientific Name", "Wikidata Q-number", "Country", "2023-01-01", "2023-01-02"]
            self.assertListEqual(sorted(df.columns), sorted(expected_columns))

            # The dataframe should contain a row for each unique species-country combination
            self.assertEqual(len(df), 4)

            # Test values for 'Pica pica'
            pica_us_row = df[(df["Scientific Name"] == "Pica pica") & (df["Country"] == "US")]
            self.assertEqual(pica_us_row["2023-01-01"].values[0], 100)
            self.assertEqual(pica_us_row["2023-01-02"].values[0], 120)

            # Test values for 'Panthera leo'
            panthera_us_row = df[(df["Scientific Name"] == "Panthera leo") & (df["Country"] == "US")]
            self.assertEqual(panthera_us_row["2023-01-01"].values[0], 20)
            # The value for 2023-01-02 is missing, so it should be NaN before fillna(0)
            # but the code fills it with 0. The test should be adjusted.
            self.assertEqual(panthera_us_row["2023-01-02"].values[0], 0)

            panthera_gb_row = df[(df["Scientific Name"] == "Panthera leo") & (df["Country"] == "GB")]
            self.assertEqual(panthera_gb_row["2023-01-01"].values[0], 50)
            self.assertEqual(panthera_gb_row["2023-01-02"].values[0], 60)

            panthera_fr_row = df[(df["Scientific Name"] == "Panthera leo") & (df["Country"] == "FR")]
            self.assertEqual(panthera_fr_row["2023-01-01"].values[0], 30)
            # The value for 2023-01-02 is missing, so it should be NaN before fillna(0)
            # The code fills it with 0. The test should be adjusted.
            self.assertEqual(panthera_fr_row["2023-01-02"].values[0], 0)

if __name__ == "__main__":
    unittest.main()