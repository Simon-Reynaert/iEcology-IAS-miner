import unittest
import pandas as pd
from unittest.mock import patch, MagicMock
from datetime import datetime, timedelta
import os

#import the function to test
from src.activity_mining.get_wiki_lang_pageviews_final import fetch_and_process_pageviews

class TestWikiPageviews(unittest.TestCase):
    def setUp(self):
        # Sample sitelinks DataFrame for mocking
        self.sitelinks_df = pd.DataFrame({
            "Scientific Name": ["Pica pica", "Panthera leo"],
            "Language": ["en", "fr"],
            "Wikipedia Title": ["Pica pica", "Panthera leo"]
        })
        self.user_agent = "TestUserAgent"
        self.output_file = "test_output.csv"
        self.start_date = "2023-01-01"
        self.end_date = "2023-01-03"
        self.all_dates = pd.date_range(
            start=self.start_date,
            end=datetime.strptime(self.end_date, "%Y-%m-%d")
        ).strftime("%Y%m%d").tolist()

        # Mock API responses for each date
        # Day 1: Pica pica gets some views
        self.api_data_day1 = {
            "items": [
                {"timestamp": "2023010100", "views": 100},
                {"timestamp": "2023010100", "views": 120}
            ]
        }
        # Day 2: Panthera leo gets some views
        self.api_data_day2 = {
            "items": [
                {"timestamp": "2023010200", "views": 50},
                {"timestamp": "2023010200", "views": 70}
            ]
        }
        # Day 3: Pica pica gets more views (simulates update)
        self.api_data_day3 = {
            "items": [
                {"timestamp": "2023010300", "views": 250}
            ]
        }
    
    @patch('src.activity_mining.get_wiki_lang_pageviews_final.os.path.exists')
    @patch('src.activity_mining.get_wiki_lang_pageviews_final.pd.read_csv')
    @patch('src.activity_mining.get_wiki_lang_pageviews_final.requests.get')
    @patch('src.activity_mining.get_wiki_lang_pageviews_final.load_dotenv')
    @patch('src.activity_mining.get_wiki_lang_pageviews_final.os.getenv')
    def test_fetch_and_process_update(self, mock_getenv, mock_load_dotenv, mock_requests_get, mock_read_csv, mock_exists):
        """
        Tests that the script correctly loads an existing file and updates it.
        """
        # The decorators apply in reverse order.
        # So the arguments need to be in reverse order as well.
        # @patch('...os.path.exists') -> mock_exists
        # @patch('...pd.read_csv')     -> mock_read_csv
        # @patch('...requests.get')    -> mock_requests_get
        # @patch('...load_dotenv')     -> mock_load_dotenv
        # @patch('...os.getenv')      -> mock_getenv

        # Mock environment variables and initial file load
        mock_getenv.return_value = self.user_agent
        
        # --- Simulate a pre-existing file ---
        mock_exists.side_effect = [
            True,  # For the initial check on the sitelinks file (if any)
            True   # For the check on the output file
        ]
        
        # Create a mock existing DataFrame with partial data
        existing_data = self.sitelinks_df.copy()
        for date_col in self.all_dates:
            existing_data[date_col] = 0
        existing_data.loc[0, '20230101'] = 100
        existing_data.loc[1, '20230102'] = 50
        
        # Mock pd.read_csv to return the sitelinks file first, then the existing data
        mock_read_csv.side_effect = [
            self.sitelinks_df.copy(),  # First call: for the sitelinks file
            existing_data              # Second call: for the output file
        ]

        # Mock API calls for the update run
        mock_requests_get.side_effect = [
            # Pica pica fetch from 20230102
            MagicMock(status_code=200, json=lambda: self.api_data_day2),
            # Panthera leo fetch from 20230103
            MagicMock(status_code=200, json=lambda: self.api_data_day3)
        ]

        # Use a list to simulate the file write
        written_dataframes = []
        with patch('src.activity_mining.get_wiki_lang_pageviews_final.pd.DataFrame.to_csv', autospec=True) as mock_to_csv:
            # We need to capture the DataFrame, not just the path.
            # So we use a lambda function to grab the first argument (the DataFrame itself).
            mock_to_csv.side_effect = lambda df, *args, **kwargs: written_dataframes.append(df)
            
            fetch_and_process_pageviews(
                sitelinks_file='sitelinks.csv',
                output_file=self.output_file,
                start_date_str=self.start_date,
                end_date_str=self.end_date
            )

        # --- Assertions for the update run ---
        self.assertEqual(len(written_dataframes), 2) # Should save after each species
        
        # Check the final state of the DataFrame after the update
        final_df = written_dataframes[-1]
        
        # The expected value for Pica pica on 20230102 is the sum of the mocked data (50 + 70 = 120)
        self.assertEqual(final_df.loc[final_df['Scientific Name'] == 'Pica pica', '20230102'].iloc[0], 120)
        self.assertEqual(final_df.loc[final_df['Scientific Name'] == 'Panthera leo', '20230103'].iloc[0], 250)
