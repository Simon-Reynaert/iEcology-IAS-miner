import unittest
from unittest.mock import patch, MagicMock
import sys
import os
import pandas as pd
import geopandas as gpd
from shapely.geometry import Polygon
import requests

# Add project root to sys.path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'src')))
from src.EASIN_mining_and_map_generation.generate_html_maps_IAS_presence_EASIN import generate_species_maps

class TestMapGeneration(unittest.TestCase):

    @patch('builtins.open', new_callable=MagicMock)  # Mock file writing
    @patch('src.EASIN_mining_and_map_generation.generate_html_maps_IAS_presence_EASIN.folium.Map')
    @patch('src.EASIN_mining_and_map_generation.generate_html_maps_IAS_presence_EASIN.pd.read_csv')
    @patch('src.EASIN_mining_and_map_generation.generate_html_maps_IAS_presence_EASIN.gpd.read_file')
    @patch('src.EASIN_mining_and_map_generation.generate_html_maps_IAS_presence_EASIN.os.path.exists', side_effect=[False, True, True])
    @patch('src.EASIN_mining_and_map_generation.generate_html_maps_IAS_presence_EASIN.os.remove')
    @patch('src.EASIN_mining_and_map_generation.generate_html_maps_IAS_presence_EASIN.zipfile.ZipFile')
    @patch('src.EASIN_mining_and_map_generation.generate_html_maps_IAS_presence_EASIN.requests.get')
    @patch('src.EASIN_mining_and_map_generation.generate_html_maps_IAS_presence_EASIN.os.makedirs')
    def test_map_generation_success(
        self, mock_makedirs, mock_requests, mock_zipfile, mock_remove, mock_exists, 
        mock_read_file, mock_read_csv, mock_map, mock_open
    ):
        """Test full map generation pipeline with mocked data."""

        # Mock request response for shapefile download
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.content = b"fake zip content"
        mock_requests.return_value = mock_response

        # Mock GeoDataFrame for countries using real Polygons
        world_gdf = gpd.GeoDataFrame({
            "CONTINENT": ["Europe", "Europe", "Asia"],
            "ISO_A2_EH": ["FR", "GB", "CN"],
            "NAME": ["France", "United Kingdom", "China"],
            "geometry": [
                Polygon([(0,0), (1,0), (1,1), (0,1)]),  # France
                Polygon([(1,1), (2,1), (2,2), (1,2)]),  # UK
                Polygon([(2,2), (3,2), (3,3), (2,3)])   # China
            ]
        })
        mock_read_file.return_value = world_gdf

        # Mock species CSV
        species_df = pd.DataFrame({
            "scientific_name": ["Species A", "Species A", "Species B"],
            "country": ["FR", "GB", "GR"],
            "present": ["Yes", "No", "Yes"]
        })
        mock_read_csv.return_value = species_df

        # Mock Folium map and save method
        mock_folium_map = MagicMock()
        mock_folium_map.get_root.return_value.html = MagicMock()
        mock_map.return_value = mock_folium_map

        # Run the function
        generate_species_maps(
            csv_file='species_by_country_presence_EASIN_updated.csv',
            shapefile_dir='test_data',
            map_output_dir='test_maps'
        )

        # Assertions
        mock_requests.assert_called_once()
        mock_zipfile.assert_called_once()
        mock_remove.assert_called_once()
        mock_read_file.assert_called_once()
        mock_read_csv.assert_called_once()
        self.assertEqual(mock_folium_map.save.call_count, 2)

        save_calls = [args[0] for args, kwargs in mock_folium_map.save.call_args_list]
        self.assertIn(os.path.join('test_maps', 'Species_A_map.html'), save_calls)
        self.assertIn(os.path.join('test_maps', 'Species_B_map.html'), save_calls)

    @patch('builtins.open', new_callable=MagicMock)  # prevents real file writing
    @patch('src.EASIN_mining_and_map_generation.generate_html_maps_IAS_presence_EASIN.os.makedirs')
    @patch(
        'src.EASIN_mining_and_map_generation.generate_html_maps_IAS_presence_EASIN.requests.get',
        side_effect=requests.exceptions.RequestException("Simulated download error")
    )
    @patch('src.EASIN_mining_and_map_generation.generate_html_maps_IAS_presence_EASIN.os.path.exists', return_value=False)
    def test_download_failure_exits_gracefully(
        self, mock_exists, mock_requests, mock_makedirs, mock_open_file
    ):
        """Test that download failures are handled gracefully."""
        with patch('sys.stdout', new_callable=MagicMock) as mock_stdout:
            generate_species_maps(
                csv_file='species_by_country_presence_EASIN_updated.csv',
                shapefile_dir='test_data',
                map_output_dir='test_maps'
            )
            printed_text = "".join(call.args[0] for call in mock_stdout.write.call_args_list)
            self.assertIn("Failed to download Natural Earth shapefile", printed_text)

if __name__ == "__main__":
    unittest.main()