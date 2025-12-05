import unittest
from unittest.mock import patch, MagicMock
import os
import sys
import glob
import pandas as pd
import numpy as np

# Add the src directory to the Python path
# Go up one level from tests/, then into src/
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'src')))

from data_processing.geolocate_process_inaturalist_data import (
    _lookup_country,
    geolocate_csv_file,
    geolocate_folder,
    join_and_pivot_geolocated
)

class TestINatProcessing(unittest.TestCase):

    # --- Unit Test for the helper function ---
    def test_lookup_country(self):
        """Test the core country lookup logic."""
        # Test with valid coordinates
        row = pd.Series({'latitude': 48.8566, 'longitude': 2.3522, 'Country': ""})
        with patch('data_processing.geolocate_process_inaturalist_data.rg.search', return_value=[{'cc': 'FR'}]):
            self.assertEqual(_lookup_country(row), 'FR')

        # Test with pre-existing country data
        row = pd.Series({'latitude': 48.8566, 'longitude': 2.3522, 'Country': "DE"})
        self.assertEqual(_lookup_country(row), 'DE')

        # Test with missing coordinates
        row = pd.Series({'latitude': np.nan, 'longitude': 2.3522, 'Country': ""})
        self.assertEqual(_lookup_country(row), "")

        # Test with lookup failure
        row = pd.Series({'latitude': 48.8566, 'longitude': 2.3522, 'Country': ""})
        with patch('data_processing.geolocate_process_inaturalist_data.rg.search', side_effect=Exception('Lookup failed')):
            self.assertEqual(_lookup_country(row), "")

    # --- Integration Test for geolocate_csv_file ---
    @patch('data_processing.geolocate_process_inaturalist_data.pd.read_csv')
    @patch('data_processing.geolocate_process_inaturalist_data.pd.DataFrame.to_csv')
    @patch('data_processing.geolocate_process_inaturalist_data.rg.search')
    def test_geolocate_csv_file(self, mock_rg_search, mock_to_csv, mock_read_csv):
        """Tests geolocate_csv_file applies geolocation and saves correctly."""
        mock_read_csv.return_value = pd.DataFrame({
            'latitude': [52.52],
            'longitude': [13.405],
            'Country': [""]
        })
        # Mock the reverse geocoder search
        mock_rg_search.return_value = [{'cc': 'DE'}]

        df = geolocate_csv_file("input.csv", "output.csv")

        # Verify rg.search was called
        mock_rg_search.assert_called()
        # Verify to_csv was called with index=False
        mock_to_csv.assert_called_once_with("output.csv", index=False)
        # Check that the returned DataFrame has the expected 'Country' values
        self.assertEqual(df['Country'].iloc[0], 'DE')

    # --- Integration Test for geolocate_folder ---
    @patch('data_processing.geolocate_process_inaturalist_data.os.makedirs')
    @patch('data_processing.geolocate_process_inaturalist_data.geolocate_csv_file')
    @patch('data_processing.geolocate_process_inaturalist_data.glob.glob')
    def test_geolocate_folder(self, mock_glob, mock_csv_file, mock_makedirs):
        """Tests geolocate_folder calls geolocate_csv_file for each CSV."""
        mock_glob.return_value = ["input_folder/file1_observations.csv", "input_folder/file2_observations.csv"]
        mock_csv_file.side_effect = [pd.DataFrame({'A':[1]}), pd.DataFrame({'A':[2]})]

        dfs = geolocate_folder("input_folder", "output_folder")
        self.assertEqual(len(dfs), 2)

        expected_path1 = os.path.join("output_folder", "file1_geolocated.csv")
        expected_path2 = os.path.join("output_folder", "file2_geolocated.csv")
        mock_csv_file.assert_any_call("input_folder/file1_observations.csv", expected_path1)
        mock_csv_file.assert_any_call("input_folder/file2_observations.csv", expected_path2)

    # --- Integration Test for join_and_pivot_geolocated ---
    @patch('data_processing.geolocate_process_inaturalist_data.glob.glob')
    @patch('data_processing.geolocate_process_inaturalist_data.pd.read_csv')
    def test_join_and_pivot_geolocated(self, mock_read_csv, mock_glob):
        """Tests join_and_pivot_geolocated combines and pivots correctly."""
        mock_glob.return_value = ["input_folder/species_A_geolocated.csv", "input_folder/species_B_geolocated.csv"]
        mock_read_csv.side_effect = [
            pd.DataFrame({'Country':['US','US'], 'observed_on':['2016-01-01','2016-01-02']}),
            pd.DataFrame({'Country':['FR'], 'observed_on':['2016-01-01']})
        ]

        final_df = join_and_pivot_geolocated("input_folder", start_date='2016-01-01', end_date='2016-01-02')

        # Check structure
        self.assertIn('Scientific Name', final_df.columns)
        self.assertIn('Country', final_df.columns)
        self.assertIn('2016-01-01', final_df.columns)
        self.assertIn('2016-01-02', final_df.columns)
        
        # Check we have two rows (one for each species-country combo)
        self.assertEqual(len(final_df), 2)
        
        # Check specific values
        species_a_row = final_df[final_df['Scientific Name'] == 'species A']
        self.assertEqual(species_a_row['Country'].iloc[0], 'US')
        self.assertEqual(species_a_row['2016-01-01'].iloc[0], 1)
        self.assertEqual(species_a_row['2016-01-02'].iloc[0], 1)
        
        species_b_row = final_df[final_df['Scientific Name'] == 'species B']
        self.assertEqual(species_b_row['Country'].iloc[0], 'FR')
        self.assertEqual(species_b_row['2016-01-01'].iloc[0], 1)
        self.assertEqual(species_b_row['2016-01-02'].iloc[0], 0)

if __name__ == "__main__":
    unittest.main()