import unittest
from unittest.mock import patch, MagicMock
import pandas as pd
import json
import sys
import os

# Add src folder to path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'src')))
from EASIN_mining_and_map_generation.get_EASIN_observations import (
    extract_coordinates, 
    fetch_occurrences, 
    save_records, 
    OUTPUT_FILE,
    get_credentials,
    main
)

class TestEasinFetcher(unittest.TestCase):

    def test_extract_coordinates_geojson(self):
        record = {"GeoJSON": '{"type": "Point", "coordinates": [12.34, 56.78]}'}
        lat, lon = extract_coordinates(record)
        self.assertEqual(lat, 56.78)
        self.assertEqual(lon, 12.34)

    def test_extract_coordinates_wkt(self):
        record = {"WKT": "POINT (12.34 56.78)"}
        lat, lon = extract_coordinates(record)
        self.assertEqual(lat, 56.78)
        self.assertEqual(lon, 12.34)

    def test_extract_coordinates_none(self):
        record = {}
        lat, lon = extract_coordinates(record)
        self.assertIsNone(lat)
        self.assertIsNone(lon)

    # -------------------------------------------------------------------------
    # The following tests are updated to mock credentials correctly.
    # -------------------------------------------------------------------------

    @patch("EASIN_mining_and_map_generation.get_EASIN_observations.requests.post")
    @patch("EASIN_mining_and_map_generation.get_EASIN_observations.get_credentials", return_value=("test_email", "test_pw"))
    def test_fetch_occurrences_success(self, mock_get_credentials, mock_post):
        # Mock a successful API response
        mock_post.return_value.json.return_value = [{"ObservationId": 1}]
        mock_post.return_value.raise_for_status = lambda: None

        # Pass mocked credentials to the function
        email, pw = mock_get_credentials()
        result = fetch_occurrences("R00001", "AT", email, pw)

        # Assertions
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]["ObservationId"], 1)
        mock_post.assert_called_once()
        self.assertEqual(mock_post.call_args[1]['json']['Email'], "test_email")
        self.assertEqual(mock_post.call_args[1]['json']['Password'], "test_pw")

    @patch("EASIN_mining_and_map_generation.get_EASIN_observations.requests.post")
    @patch("EASIN_mining_and_map_generation.get_EASIN_observations.get_credentials", return_value=("test_email", "test_pw"))
    def test_fetch_occurrences_empty(self, mock_get_credentials, mock_post):
        # Mock empty API response
        mock_post.return_value.json.return_value = {"Empty": "No results"}
        mock_post.return_value.raise_for_status = lambda: None

        # Pass mocked credentials to the function
        email, pw = mock_get_credentials()
        result = fetch_occurrences("R00002", "BE", email, pw)

        # Assertion
        self.assertEqual(result, [])
        mock_post.assert_called_once()

    # -------------------------------------------------------------------------
    # New test to verify the credential validation logic.
    # -------------------------------------------------------------------------
    @patch("EASIN_mining_and_map_generation.get_EASIN_observations.load_dotenv")
    @patch.dict("os.environ", {}, clear=True)
    def test_get_credentials_raises_error(self,mock_load_dotenv):
        with self.assertRaises(ValueError) as cm:
            get_credentials()
        self.assertIn("Please set EASIN_EMAIL and EASIN_PW", str(cm.exception))

    # -------------------------------------------------------------------------
    # Main workflow test is updated to mock get_credentials()
    # -------------------------------------------------------------------------
    
    @patch("EASIN_mining_and_map_generation.get_EASIN_observations.get_credentials", return_value=("test_email", "test_pw"))
    @patch("EASIN_mining_and_map_generation.get_EASIN_observations.pd.read_csv")
    @patch("EASIN_mining_and_map_generation.get_EASIN_observations.save_records")
    @patch("EASIN_mining_and_map_generation.get_EASIN_observations.fetch_occurrences")
    @patch("os.path.exists")
    @patch("EASIN_mining_and_map_generation.get_EASIN_observations.pd.DataFrame.to_csv")
    @patch("time.sleep", lambda x: None)  # Avoid actual sleep
    def test_main_workflow(self, mock_to_csv, mock_exists, mock_fetch, mock_save, mock_read_csv, mock_get_credentials):
        # Mock species CSV
        mock_read_csv.return_value = pd.DataFrame({"EASIN.ID": ["R00001", "R00002"]})
        mock_exists.return_value = False  # No existing output file
        mock_fetch.return_value = [{"ObservationId": 1}]

        # Call the refactored main function
        main()

        # Ensure fetch_occurrences was called for each species and country
        self.assertTrue(mock_fetch.called)
        self.assertTrue(mock_save.called)
        self.assertTrue(mock_to_csv.called)
        self.assertTrue(mock_get_credentials.called)


if __name__ == "__main__":
    unittest.main()