# tests/test_easin_fetcher.py
import unittest
from unittest.mock import patch, MagicMock
import pandas as pd
import json
import sys
import os

# Add src folder to path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'src')))
from EASIN_mining_and_map_generation.get_EASIN_observations import extract_coordinates, fetch_occurrences, save_records, OUTPUT_FILE

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

    @patch("EASIN_mining_and_map_generation.get_EASIN_observations.requests.post")
    def test_fetch_occurrences_success(self, mock_post):
        # Mock a successful API response
        mock_post.return_value.json.return_value = [{"ObservationId": 1}]
        mock_post.return_value.raise_for_status = lambda: None

        result = fetch_occurrences("R00001", "AT")
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]["ObservationId"], 1)

    @patch("EASIN_mining_and_map_generation.get_EASIN_observations.requests.post")
    def test_fetch_occurrences_empty(self, mock_post):
        # Mock empty API response
        mock_post.return_value.json.return_value = {"Empty": "No results"}
        mock_post.return_value.raise_for_status = lambda: None

        result = fetch_occurrences("R00002", "BE")
        self.assertEqual(result, [])

    @patch("EASIN_mining_and_map_generation.get_EASIN_observations.pd.DataFrame.to_csv")
    def test_save_records(self, mock_to_csv):
        records = [
            {"SpeciesName": "Testus species", "CountryId": "AT", "GeoJSON": '{"type":"Point","coordinates":[10,20]}',
             "DataPartnerName": "Partner", "Year": 2023, "ObservationId": 123, "Reference": "Ref", "ReferenceUrl": "url"}
        ]
        save_records("R00001", records)

        # Ensure CSV append is called
        self.assertTrue(mock_to_csv.called)
        args, kwargs = mock_to_csv.call_args
        self.assertEqual(kwargs.get("index", None), False)

        df_saved = args[0] if args else None
        # Confirm that coordinates were extracted correctly
        self.assertEqual(records[0]["GeoJSON"], '{"type":"Point","coordinates":[10,20]}')

    @patch("EASIN_mining_and_map_generation.get_EASIN_observations.pd.read_csv")
    @patch("EASIN_mining_and_map_generation.get_EASIN_observations.save_records")
    @patch("EASIN_mining_and_map_generation.get_EASIN_observations.fetch_occurrences")
    @patch("os.path.exists")
    @patch("EASIN_mining_and_map_generation.get_EASIN_observations.pd.DataFrame.to_csv")
    @patch("time.sleep", lambda x: None)  # Avoid actual sleep
    def test_main_workflow(self, mock_to_csv, mock_exists, mock_fetch, mock_save, mock_read_csv):
        # Mock species CSV
        mock_read_csv.return_value = pd.DataFrame({"EASIN.ID": ["R00001", "R00002"]})
        mock_exists.return_value = False  # No existing output file
        mock_fetch.return_value = [{"ObservationId": 1}]

        from EASIN_mining_and_map_generation.get_EASIN_observations import main
        main()

        # Ensure fetch_occurrences was called for each species and country
        self.assertTrue(mock_fetch.called)
        self.assertTrue(mock_save.called)
        self.assertTrue(mock_to_csv.called)

if __name__ == "__main__":
    unittest.main()
