import unittest
from unittest.mock import patch, MagicMock
import os
import sys
import glob
import pandas as pd
import numpy as np

# Add the parent directory to the Python path for module discovery.
# This assumes the script is located in src/ and the tests in tests/.
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'src')))

from data_processing.geolocate_process_inaturalist_data import _lookup_country, geolocate_and_save_files, join_and_pivot_data

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

    # --- Integration Test for the first pipeline step ---
    @patch('data_processing.geolocate_process_inaturalist_data.os.path.join', side_effect=lambda *args: '/'.join(args))
    @patch('data_processing.geolocate_process_inaturalist_data.glob.glob')
    @patch('data_processing.geolocate_process_inaturalist_data.pd.read_csv')
    @patch('data_processing.geolocate_process_inaturalist_data.pd.DataFrame.apply')
    @patch('data_processing.geolocate_process_inaturalist_data.pd.DataFrame.to_csv')
    def test_geolocate_and_save_files(self, mock_to_csv, mock_apply, mock_read_csv, mock_glob, mock_join):
        """Tests that geolocation is applied and files are saved correctly."""
        # Mock glob to return a list of fake files
        mock_glob.return_value = ["test_folder/species_A_observations.csv", "test_folder/species_B_observations.csv"]
        
        # Mock the DataFrame that read_csv will return
        mock_read_csv.return_value = pd.DataFrame({
            'latitude': [52.5200],
            'longitude': [13.4050],
            'Country': [""]
        })
        
        # Mock the `apply` method to return a pre-determined result
        mock_apply.return_value = pd.Series(["DE"])

        # Execute the function
        geolocate_and_save_files("test_folder")
        
        # Verify that read_csv was called for both files
        self.assertEqual(mock_read_csv.call_count, 2)
        
        # Verify that the geolocate function was applied
        mock_apply.assert_called()

        # Verify that to_csv was called for both new files
        self.assertEqual(mock_to_csv.call_count, 2)
        
        # Verify that the correct output paths were used
        save_calls = [args[0] for args, kwargs in mock_to_csv.call_args_list]
        self.assertIn('test_folder/species_A_geolocated.csv', save_calls)
        self.assertIn('test_folder/species_B_geolocated.csv', save_calls)

    # --- Integration Test for the second pipeline step ---
    @patch('data_processing.geolocate_process_inaturalist_data.os.path.join', side_effect=lambda *args: '/'.join(args))
    @patch('data_processing.geolocate_process_inaturalist_data.glob.glob')
    @patch('data_processing.geolocate_process_inaturalist_data.pd.read_csv')
    @patch('data_processing.geolocate_process_inaturalist_data.pd.DataFrame.to_csv')
    @patch('data_processing.geolocate_process_inaturalist_data.datetime')
    def test_join_and_pivot_data(self, mock_datetime, mock_to_csv, mock_read_csv, mock_glob, mock_join):
        """Tests that files are joined, pivoted, and saved correctly."""
        # Set a fixed date for today's date to make the date range predictable
        mock_datetime.today.return_value = pd.to_datetime('2016-01-02')

        # Mock glob to return a list of fake geolocated files
        mock_glob.return_value = ["test_folder/species_A_geolocated.csv", "test_folder/species_B_geolocated.csv"]
        
        # Mock the DataFrames that read_csv will return for each file
        mock_read_csv.side_effect = [
            # Data for species A
            pd.DataFrame({
                'Country': ['US', 'US'],
                'observed_on': ['2016-01-01', '2016-01-02'],
            }),
            # Data for species B
            pd.DataFrame({
                'Country': ['FR'],
                'observed_on': ['2016-01-01'],
            }),
        ]

        # Execute the function
        final_df = join_and_pivot_data("test_folder")
        
        # Verify that read_csv was called twice
        self.assertEqual(mock_read_csv.call_count, 2)
        
        # Create the expected final DataFrame to compare against
        expected_data = {
            'Scientific Name': ['species A', 'species B'],
            'Country': ['US', 'FR'],
            '2016-01-01': [1, 1],
            '2016-01-02': [1, 0]
        }
        expected_df = pd.DataFrame(expected_data).sort_values(by=['Scientific Name', 'Country']).reset_index(drop=True)

        # Set the column name attribute on the expected DataFrame to match the pivot_table output
        expected_df.columns.name = 'date_str'

        # Assert that the output DataFrame matches the expected DataFrame, ignoring dtype
        pd.testing.assert_frame_equal(final_df, expected_df, check_dtype=False)
