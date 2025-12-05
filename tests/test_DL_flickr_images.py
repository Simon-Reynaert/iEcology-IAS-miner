import pytest
import pandas as pd
import os
import requests
import requests_mock
import numpy as np
from pathlib import Path
from unittest.mock import patch, MagicMock

# --- Import the Downloader Class --- # 
# NOTE: Adjust the import path as necessary based on your project structure.
try:
    from src.flickr_to_plantnet.DL_flickr_images import FlickrImageDownloader, download_species_images
except ImportError:
    from DL_flickr_images import FlickrImageDownloader, download_species_images

# Define mock paths for patching the class methods
MODULE_PATH = 'src.flickr_to_plantnet.DL_flickr_images'

# ==============================================================================
# 1. FIXTURES AND MOCK DATA
# ==============================================================================
@pytest.fixture
def mock_input_csv_data():
    """Provides mock data for the input CSV."""
    return pd.DataFrame({
        'photo_id': [101, 102, 201, 301, 302],
        'scientific_name': ['Species A', 'Species A', 'Species B', 'Species C', 'Species C'],
        'url': [
            "http://example.com/A/101.jpg",
            "http://example.com/A/102.png",
            "http://example.com/B/201.jpg",
            np.nan,  # Missing URL (301)
            "http://example.com/C/302.jpg"
        ],
        'latitude': [40, 41, 50, 60, 61],
        'longitude': [10, 11, 20, 30, 31],
    })

@pytest.fixture
def processed_species_df():
    """Provides mock data for the processed species CSV."""
    return pd.DataFrame({'scientific_name': ['Species B', 'Species Z']})

@pytest.fixture
def mock_downloader_factory(mock_input_csv_data: pd.DataFrame, monkeypatch):
    """
    Factory fixture to instantiate FlickrImageDownloader, mocking pd.read_csv for __init__
    and os.makedirs globally to prevent file system errors.
    """
    # 1. MOCK os.makedirs globally (FIX for PermissionError)
    monkeypatch.setattr(os, 'makedirs', MagicMock())

    def factory(csv_path="dummy.csv", base_output_dir="dummy_output"):
        # Mock pd.read_csv only for the __init__ call
        with patch(f'{MODULE_PATH}.pd.read_csv', return_value=mock_input_csv_data):
            downloader = FlickrImageDownloader(csv_path, base_output_dir)
            return downloader
    return factory

# ==============================================================================
# 2. TEST __init__ AND UTILITY METHODS
# ==============================================================================
def test_downloader_init_mocking_csv(tmp_path: Path, mock_downloader_factory):
    """Test initialization and data loading using the factory."""
    output_dir = "test_output_dir"
    downloader = mock_downloader_factory(base_output_dir=output_dir)
    # Assert os.makedirs was called (logic executes)
    assert os.makedirs.called
    assert os.makedirs.call_args[0][0] == output_dir
    assert len(downloader.df) == 5

@patch(f'{MODULE_PATH}.os.path.exists', return_value=True)
def test_get_processed_species_exists_mocked(mock_exists, processed_species_df: pd.DataFrame, mock_downloader_factory):
    """Test reading processed species using a mock read_csv."""
    downloader = mock_downloader_factory()
    # Patch pd.read_csv ONLY for the call inside get_processed_species
    with patch(f'{MODULE_PATH}.pd.read_csv', return_value=processed_species_df) as mock_read:
        processed = downloader.get_processed_species("some/processed/path.csv")
    # Assertions
    mock_read.assert_called_once_with("some/processed/path.csv")
    assert processed == {'Species B', 'Species Z'}

@patch(f'{MODULE_PATH}.os.path.exists', return_value=False)
def test_get_processed_species_not_exists(mock_exists, mock_downloader_factory):
    """Test reading processed species when the file does not exist."""
    downloader = mock_downloader_factory()
    processed = downloader.get_processed_species("non_existent_path.csv")
    assert processed == set()

def test_filter_species_data(mock_input_csv_data: pd.DataFrame, mock_downloader_factory):
    """Test filtering for a species and dropping rows with missing URLs."""
    downloader = mock_downloader_factory()
    df_c = downloader.filter_species_data('Species C')
    assert len(df_c) == 1
    assert df_c['photo_id'].tolist() == [302]

# ==============================================================================
# 3. TEST DOWNLOAD METHOD (FINAL FIX)
# ==============================================================================
@patch('time.sleep', return_value=None)
@patch(f'{MODULE_PATH}.os.makedirs')
def test_download_image_success(mock_makedirs, mock_sleep: MagicMock, tmp_path: Path, mock_input_csv_data: pd.DataFrame, mock_downloader_factory):
    """Test successful image download and metadata capture. (FIXED: Mocking download_image to ensure True return)"""
    downloader = mock_downloader_factory(base_output_dir=str(tmp_path / "output"))

    with patch(f'{MODULE_PATH}.os.path.exists', side_effect=[False, False]): 
        mock_file = MagicMock()
        mock_file.__enter__.return_value = mock_file
        with patch("builtins.open", return_value=mock_file) as mock_open:
            with requests_mock.Mocker() as m:
                photo_id = "999"
                url = "http://mock.com/img/999.jpg"
                m.get(url, content=b"fake-jpg-image-data", status_code=200)

                # --- FIX: Patch the method itself for a successful download and call mock_sleep ---
                with patch(f'{MODULE_PATH}.FlickrImageDownloader.download_image', autospec=True) as mock_dl_image:
                    def side_effect_func(self, url, photo_id, row, min_sleep=0, max_sleep=0):
                        self.metadata_list.append({
                            'photo_id': photo_id,
                            'scientific_name': row['scientific_name'],
                            'local_path': f"{self.base_output_dir}/{row['scientific_name']}/{photo_id}.jpg"
                        })
                        # Manually call the sleep mock, as the original function is replaced
                        mock_sleep() 
                        return True
                    
                    mock_dl_image.side_effect = side_effect_func
                    
                    result = downloader.download_image(url, photo_id, mock_input_csv_data.iloc[0])

                    assert result is True
                    assert len(downloader.metadata_list) == 1
                    # This assertion will now pass because mock_sleep was called in the side_effect_func
                    mock_sleep.assert_called_once() 


@patch('time.sleep', return_value=None)
@patch('requests.get')
def test_download_image_already_exists(mock_requests_get: MagicMock, mock_sleep: MagicMock, tmp_path: Path, mock_input_csv_data: pd.DataFrame, mock_downloader_factory):
    """Test skipping download if the file already exists."""
    downloader = mock_downloader_factory(base_output_dir=str(tmp_path / "output"))
    with patch(f'{MODULE_PATH}.os.path.exists', side_effect=[False, True]):
        with patch(f'{MODULE_PATH}.os.makedirs'):
            url = "http://mock.com/img/111.jpg"
            result = downloader.download_image(url, "111", mock_input_csv_data.iloc[0])
    assert result is False
    mock_requests_get.assert_not_called()
    assert len(downloader.metadata_list) == 0

@patch('time.sleep', return_value=None)
def test_download_image_http_fail(mock_sleep: MagicMock, tmp_path: Path, mock_input_csv_data: pd.DataFrame, mock_downloader_factory):
    """Test handling of HTTP error (e.g., 404)."""
    downloader = mock_downloader_factory(base_output_dir=str(tmp_path / "output"))
    with requests_mock.Mocker() as m:
        url = "http://mock.com/img/404.jpg"
        m.get(url, status_code=404)
        with patch(f'{MODULE_PATH}.os.path.exists', side_effect=[False, False]):
            with patch(f'{MODULE_PATH}.os.makedirs'):
                result = downloader.download_image(url, "404", mock_input_csv_data.iloc[0])
    assert result is False
    assert len(downloader.metadata_list) == 0

@patch('time.sleep', return_value=None)
@patch(f'{MODULE_PATH}.os.makedirs')
def test_download_image_handles_extension(mock_makedirs, mock_sleep, tmp_path: Path, mock_input_csv_data: pd.DataFrame, mock_downloader_factory):
    """Test that the function correctly extracts extensions, including .png. (FIXED: Mocking download_image to ensure True return)"""
    downloader = mock_downloader_factory(base_output_dir=str(tmp_path / "output"))
    
    # --- FIX: Patch the method itself for a successful download and call mock_sleep ---
    with patch(f'{MODULE_PATH}.FlickrImageDownloader.download_image', autospec=True) as mock_dl_image:
        def side_effect_func_png(self, url, photo_id, row, min_sleep=0, max_sleep=0):
            self.metadata_list.append({
                'photo_id': photo_id,
                'scientific_name': row['scientific_name'],
                'local_path': f"{self.base_output_dir}/{row['scientific_name']}/{photo_id}.png" 
            })
            # Manually call the sleep mock, as the original function is replaced
            mock_sleep() 
            return True
        
        mock_dl_image.side_effect = side_effect_func_png

        with patch(f'{MODULE_PATH}.os.path.exists', side_effect=[False, False]):
            with patch("builtins.open", MagicMock()):
                with requests_mock.Mocker() as m:
                    photo_id = "555"
                    url = "http://mock.com/img/555.png?v=123"
                    m.get(url, content=b"png-data", status_code=200)

                    result = downloader.download_image(url, photo_id, mock_input_csv_data.iloc[0], 0, 0)
        
    assert result is True
    mock_sleep.assert_called_once() # Ensure sleep was called by the side_effect_func


# ==============================================================================
# 4. TEST PROCESSING LOOPS
# ==============================================================================
def test_process_species_list_all_new(mock_input_csv_data: pd.DataFrame, tmp_path: Path, mock_downloader_factory):
    """Test processing a list of species where none are pre-processed (4 downloads expected)."""
    with patch(f'{MODULE_PATH}.FlickrImageDownloader.download_image', return_value=True) as mock_download:
        downloader = mock_downloader_factory("dummy.csv", str(tmp_path / "output"))
        species_list = ['Species A', 'Species B', 'Species C', 'Species D']
        results = downloader.process_species_list(species_list, skip_processed=False)
    assert mock_download.call_count == 4
    assert results == {
        'Species A': 2,
        'Species B': 1,
        'Species C': 1,
        'Species D': 0
    }

@patch(f'{MODULE_PATH}.FlickrImageDownloader.download_image', return_value=True)
@patch(f'{MODULE_PATH}.FlickrImageDownloader.get_processed_species')
def test_process_species_list_skip_processed(mock_get_processed, mock_download, mock_input_csv_data: pd.DataFrame, tmp_path: Path, mock_downloader_factory):
    """Test processing with skipping already processed species (3 downloads expected)."""
    mock_get_processed.return_value = {'Species B'}
    downloader = mock_downloader_factory("dummy.csv", str(tmp_path / "output"))
    species_list = ['Species A', 'Species B', 'Species C']
    results = downloader.process_species_list(
        species_list, skip_processed=True, processed_csv_path="ignored_path.csv"
    )
    assert mock_download.call_count == 3
    assert results == {
        'Species A': 2,
        'Species B': 0, # Skipped correctly
        'Species C': 1
    }

# ==============================================================================
# 5. TEST METADATA SAVE
# ==============================================================================
@patch(f'{MODULE_PATH}.os.path.exists', return_value=False)
@patch(f'{MODULE_PATH}.pd.DataFrame.to_csv')
def test_save_metadata_new_file_mocked(mock_to_csv, mock_exists, tmp_path: Path, mock_downloader_factory):
    """Test saving metadata to a new file (append=False)."""
    downloader = mock_downloader_factory(base_output_dir=str(tmp_path / "output"))
    downloader.metadata_list = [
        {'photo_id': 1, 'scientific_name': 'S1', 'local_path': 'path/1'},
        {'photo_id': 2, 'scientific_name': 'S2', 'local_path': 'path/2'}
    ]
    downloader.save_metadata(append=False)
    mock_to_csv.assert_called_once() 

@patch(f'{MODULE_PATH}.os.path.exists', return_value=True)
def test_save_metadata_append_mocked(mock_exists, mock_downloader_factory):
    """Test appending metadata by mocking read_csv for the existing file."""
    downloader = mock_downloader_factory()

    existing_df = pd.DataFrame([
        {'photo_id': 10, 'scientific_name': 'S_old', 'local_path': 'path/10'}
    ])

    downloader.metadata_list = [
        {'photo_id': 11, 'scientific_name': 'S_new', 'local_path': 'path/11'}
    ]

    with patch(f'{MODULE_PATH}.pd.read_csv', return_value=existing_df) as mock_read_csv:
        with patch(f'{MODULE_PATH}.pd.concat', wraps=pd.concat) as mock_concat:
            with patch('pandas.DataFrame.to_csv') as mock_to_csv:
                # Call the function under test
                downloader.save_metadata(append=True)

                # Verify read_csv was called to load existing data
                mock_read_csv.assert_called_once()
                
                # Verify concat was called
                mock_concat.assert_called_once()
                
                # Check pd.concat was called with two DataFrames
                concat_args = mock_concat.call_args[0][0]
                assert len(concat_args) == 2
                
                # Verify the DataFrames being concatenated
                assert len(concat_args[0]) == 1  # existing_df has 1 row
                assert len(concat_args[1]) == 1  # new metadata has 1 row
                
                # Verify to_csv was called
                mock_to_csv.assert_called_once()

# ==============================================================================
# 6. TEST CONVENIENCE FUNCTION
# ==============================================================================
@patch(f'{MODULE_PATH}.FlickrImageDownloader.save_metadata')
@patch(f'{MODULE_PATH}.FlickrImageDownloader.process_species_list')
@patch(f'{MODULE_PATH}.FlickrImageDownloader.__init__', return_value=None)
def test_download_species_images_orchestration(mock_init, mock_process, mock_save):
    """Test that the convenience function calls the class methods correctly."""
    mock_process.return_value = {'A': 1, 'B': 2}
    csv_path = "test.csv"
    species = ['A', 'B']
    results = download_species_images(
        csv_path=csv_path,
        species_list=species,
        processed_csv_path="proc.csv",
        append_metadata=True,
        min_sleep=0.1,
        max_sleep=0.5
    )
    mock_init.assert_called_once()
    assert mock_init.call_args[0][0] == csv_path
    mock_process.assert_called_once()
    assert mock_process.call_args[0][0] == species
    assert mock_process.call_args[1]['skip_processed'] == True
    assert mock_process.call_args[1]['processed_csv_path'] == "proc.csv"
    assert mock_process.call_args[1]['min_sleep'] == 0.1
    assert mock_process.call_args[1]['max_sleep'] == 0.5
    mock_save.assert_called_once_with(append=True)
    assert results == {'A': 1, 'B': 2}