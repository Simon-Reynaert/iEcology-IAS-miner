import unittest
from unittest.mock import patch, MagicMock, call
import pandas as pd
import os
import io

# --- IMPORTANT: Ensure the module path is correct ---
# This assumes your main script is accessible via the path: src.list_mining.get_synonyms_GBIF
from src.list_mining.get_synonyms_GBIF import (
    get_gbif_species_key, 
    get_gbif_synonyms, 
    get_gbif_common_names, 
    process_species,
    fetch_gbif_names_and_synonyms,
    main
)

# Define the full module path for patching purposes
MODULE_PATH = 'src.list_mining.get_synonyms_GBIF'

class TestGBIFProcessor(unittest.TestCase):

    # --- Setup and Teardown ---
    def setUp(self):
        """Set up environment and mock data."""
        self.scientific_name_accepted = "Anodonta anatina"
        self.scientific_name_no_match = "NonExistentium Species"
        self.key_accepted = 4559631
        self.mock_synonyms = [
            {"scientificName": "Anodonta acallia"},
            {"scientificName": "Anodonta adusta"},
            {"scientificName": "Anodon subrhombea"}
        ]
        self.mock_vernacular = [
            {"vernacularName": "Duck Mussel", "language": "en"},
            {"vernacularName": "Kaczka-perłopiórka", "language": "pl"},
            {"vernacularName": "Éti kagyló", "language": "hun"} 
        ]
        
        # Define mock input data for pandas.read_csv
        self.mock_input_df = pd.DataFrame({
            "Scientific Name": [self.scientific_name_accepted, self.scientific_name_no_match],
            "Other Data": [100, 200]
        })
        self.input_file = "test_input.csv"
        self.output_file = "test_output.csv"

    def tearDown(self):
        """No file cleanup needed since files are mocked."""
        pass # Keeping this for clarity, but the original file cleanup is gone

    # --- Unit Tests for Core Functions (Patched pygbif) ---

    @patch('pygbif.species.name_backbone')
    def test_get_gbif_species_key_success(self, mock_backbone):
        """Test successful key retrieval for an accepted name."""
        mock_backbone.return_value = {"usageKey": self.key_accepted, "status": "ACCEPTED"}
        key = get_gbif_species_key("Accepted Name")
        self.assertEqual(key, self.key_accepted)

    @patch('pygbif.species.name_backbone')
    def test_get_gbif_species_key_no_match(self, mock_backbone):
        """Test key retrieval for a name that returns NO_MATCH."""
        mock_backbone.return_value = {"status": "NO_MATCH"}
        key = get_gbif_species_key("No Match")
        self.assertIsNone(key)

    @patch('pygbif.species.name_usage')
    def test_get_gbif_synonyms(self, mock_usage):
        """Test synonym retrieval and correct extraction."""
        mock_usage.return_value = {"results": self.mock_synonyms}
        syns = get_gbif_synonyms(self.key_accepted)
        expected_syns = {"Anodonta acallia", "Anodonta adusta", "Anodon subrhombea"}
        self.assertEqual(syns, expected_syns)

    @patch('pygbif.species.name_usage')
    def test_get_gbif_common_names(self, mock_usage):
        """Test common name retrieval, language filtering, and tuple format."""
        mock_usage.return_value = {"results": self.mock_vernacular}
        names = get_gbif_common_names(self.key_accepted)
        
        # Expected names are filtered to the ALLOWED_LANGS set
        expected_names = {
            ("Duck Mussel", "en"), 
            ("Kaczka-perłopiórka", "pl"),
            ("Éti kagyló", "hun")
        }
        self.assertEqual(names, expected_names)

    # --- Integration Test for processing a single species (Patched local functions) ---

    @patch(f'{MODULE_PATH}.get_gbif_species_key')
    @patch(f'{MODULE_PATH}.get_gbif_synonyms')
    @patch(f'{MODULE_PATH}.get_gbif_common_names')
    def test_process_species_successful(self, mock_names, mock_syns, mock_key):
        """Test the combined processing function for a successful case."""
        mock_key.return_value = self.key_accepted
        mock_syns.return_value = {"S1", "S2"}
        mock_names.return_value = {("N1", "en"), ("N2", "fr")}

        result = process_species(self.scientific_name_accepted)
        
        self.assertIsNotNone(result)
        self.assertEqual(result["scientific_name"], self.scientific_name_accepted)

    # --- Integration Test for the Main Workflow (Full Mocking of I/O) ---

    @patch(f'{MODULE_PATH}.process_species')
    @patch('pandas.DataFrame.to_csv') # Mock output writing
    @patch('pandas.read_csv')         # Mock input reading
    def test_fetch_gbif_names_and_synonyms_integration(self, mock_read_csv, mock_to_csv, mock_process):
        """Test the full workflow, including mocked CSV I/O and result formatting."""
        
        # Setup mock input reading
        mock_read_csv.return_value = self.mock_input_df
        
        # Setup mock results for the two species in the test CSV
        mock_process.side_effect = [
            {
                "scientific_name": self.scientific_name_accepted,
                "synonyms": ["SynA", "SynB"],
                "common_names": [("NameX", "es"), ("NameY", "de")]
            },
            # Second species (no match) returns None from process_species
            None 
        ]

        df_result = fetch_gbif_names_and_synonyms(self.input_file, self.output_file, max_workers=1)
        
        self.assertIsNotNone(df_result)
        
        # 1. Verify read_csv was called correctly
        mock_read_csv.assert_called_once_with(self.input_file, usecols=['Scientific Name'])
        
        # 2. Verify process_species was called for each unique species
        self.assertEqual(mock_process.call_count, 2)
        mock_process.assert_has_calls([
            call(self.scientific_name_accepted),
            call(self.scientific_name_no_match)
        ], any_order=True)

        # 3. Verify output formatting and to_csv call
        self.assertEqual(len(df_result), 4) # 2 synonyms + 2 common names
        
        mock_to_csv.assert_called_once()
        
        # Check arguments passed to to_csv
        args, kwargs = mock_to_csv.call_args
        self.assertEqual(args[0], self.output_file) # Check filename
        self.assertEqual(kwargs['index'], False)    # Check index=False
        self.assertEqual(kwargs['encoding'], 'utf-8') # Check encoding='utf-8'


    # --- Test for Standalone Execution ---
    
    @patch(f'{MODULE_PATH}.fetch_gbif_names_and_synonyms')
    @patch('builtins.print')
    def test_main_function_default_files(self, mock_print, mock_fetch):
        """Test that main() calls the fetch function with correct default parameters."""
        
        # Define the expected defaults from your script's main()
        expected_input = "species_wikipedia_sitelinks.csv"
        # CORRECTED: Match the actual default in your main() function.
        expected_output = "GBIF_unionlist_synonyms.csv" 
        expected_workers = 10

        main()

        # Check that fetch_gbif_names_and_synonyms was called correctly
        mock_fetch.assert_called_once_with(
            input_csv_path=expected_input,
            output_csv_path=expected_output,
            max_workers=expected_workers
        )

# --- Execution Block ---

if __name__ == '__main__':
    # Run the tests
    unittest.main(argv=['first-arg-is-ignored'], exit=False)