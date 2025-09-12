import unittest
from unittest.mock import patch, MagicMock, mock_open
import sys
import os
import requests

# Correct the path to import the script.
# This adds the parent directory's 'src' to the Python path.
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'src')))

# Correctly import the module from the new directory structure.
from list_mining.get_EASIN_unionlistofconcern import fetch_and_process_easin_data

class TestEasinApiProcessing(unittest.TestCase):
    
    @patch("list_mining.get_EASIN_unionlistofconcern.requests.get")
    @patch("list_mining.get_EASIN_unionlistofconcern.open", new_callable=mock_open)
    def test_fetch_and_process_success(self, mock_file, mock_requests_get):
        """
        Tests that the script correctly fetches, processes, and writes data
        from a successful API response.
        """
        # --- Mock a successful API response ---
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.json.return_value = [
            {
                "Name": "Species A",
                "CommonNames": [
                    {"Name": "Common Name A1"},
                    {"Name": "Common Name A2"}
                ],
                "Synonyms": [
                    {"Synonym": "Synonym A1"},
                    {"Synonym": "Synonym A2"}
                ]
            },
            {
                "Name": "Species B",
                "CommonNames": [],
                "Synonyms": [
                    {"Synonym": "Synonym B1"}
                ]
            }
        ]
        mock_requests_get.return_value = mock_response

        # --- Execute the function ---
        test_url = "http://test-api.com"
        test_output_file = "test_output.csv"
        fetch_and_process_easin_data(test_url, test_output_file)

        # --- Verify the behavior and output ---
        
        # Check that the API was called with the correct URL
        mock_requests_get.assert_called_once_with(test_url)

        # Check that the file was opened for writing
        mock_file.assert_called_once_with(test_output_file, mode='w', newline='', encoding='utf-8')

        # Check the data that was written to the file
        written_data = mock_file().write.call_args_list
        
        # Expected rows including the header
        expected_rows = [
            'Scientific Name,Label,All Names\r\n',
            'Species A,Common Name,Common Name A1\r\n',
            'Species A,Common Name,Common Name A2\r\n',
            'Species A,Synonym,Synonym A1\r\n',
            'Species A,Synonym,Synonym A2\r\n',
            'Species B,Synonym,Synonym B1\r\n'
        ]

        # Verify each line was written correctly
        for i, row in enumerate(expected_rows):
            self.assertEqual(written_data[i][0][0], row)

    @patch("list_mining.get_EASIN_unionlistofconcern.requests.get", side_effect=requests.exceptions.RequestException)
    @patch("list_mining.get_EASIN_unionlistofconcern.open")
    def test_fetch_and_process_failure(self, mock_file, mock_requests_get):
        """
        Tests that the script handles a failed API call gracefully.
        """
        test_url = "http://test-api.com"
        test_output_file = "test_output.csv"
        
        # Patch sys.stdout to capture printed output
        with patch('sys.stdout', new_callable=MagicMock) as mock_stdout:
            fetch_and_process_easin_data(test_url, test_output_file)
            
            # Verify that the file was never opened
            mock_file.assert_not_called()
            
            # Get the arguments that were passed to the write() method
            # `call_args_list` returns a list of tuples, where each tuple is `(args, kwargs)`.
            captured_output_calls = mock_stdout.write.call_args_list
            
            # Convert the captured calls to a single string for easier assertion.
            # We're checking for the presence of the error message, not the exact string.
            full_captured_output = "".join([call[0][0] for call in captured_output_calls])
            
            # Verify that the failure message was printed
            self.assertIn("Error: Unable to fetch data from the API", full_captured_output)
