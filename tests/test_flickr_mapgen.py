import pytest
import pandas as pd
import os
from unittest.mock import MagicMock, patch
import numpy as np

# --- Import from the specified module/path ---
# NOTE: Adjust MODULE_PATH if your import structure is different.
MODULE_PATH = 'src.flickr_to_plantnet.generate_observation_map'

try:
    from src.flickr_to_plantnet.generate_observation_map import (
        load_data_for_mapping, 
        calculate_auto_extent, 
        plot_observations_on_map, 
        generate_observation_map,
        PLOT_CONFIG
    )
except ImportError:
    import sys
    sys.path.append(os.path.dirname(__file__))
    from generate_observation_map import (
        load_data_for_mapping, 
        calculate_auto_extent, 
        plot_observations_on_map, 
        generate_observation_map,
        PLOT_CONFIG
    )

# --- Fixtures (Unchanged) ---
@pytest.fixture
def mock_valid_data(tmp_path):
    data = {
        'latitude': [48.8584, 51.5074, 40.7128, 48.8584],
        'longitude': [2.2945, -0.1278, -74.0060, 2.2945],
        'scientific_name': ['Species A', 'Species B', 'Species A', 'Species A'],
        'PlantNet_Confidence_Score': [0.95, 0.75, 0.88, 0.60]
    }
    df = pd.DataFrame(data)
    file_path = tmp_path / "valid_data.csv"
    df.to_csv(file_path, index=False)
    return file_path, df

@pytest.fixture
def mock_data_with_errors(tmp_path):
    data = {
        'latitude': [48.85, 'bad_lat', 40.71, np.nan],
        'longitude': [2.29, -0.12, 'bad_lon', -74.00],
        'scientific_name': ['Species A', 'Species B', 'Species C', 'Species D'],
        'PlantNet_Confidence_Score': [0.95, 0.75, 0.88, 0.60]
    }
    df = pd.DataFrame(data)
    file_path = tmp_path / "data_with_errors.csv"
    df.to_csv(file_path, index=False)
    return file_path, pd.DataFrame(data).iloc[[0]] 

@pytest.fixture
def mock_empty_data(tmp_path):
    df = pd.DataFrame(columns=['latitude', 'longitude', 'scientific_name', 'PlantNet_Confidence_Score'])
    file_path = tmp_path / "empty_data.csv"
    df.to_csv(file_path, index=False)
    return file_path, df

# --- Test load_data_for_mapping function (Unchanged) ---
def test_load_data_valid(mock_valid_data):
    file_path, expected_df = mock_valid_data
    df = load_data_for_mapping(str(file_path))
    assert len(df) == 4
    assert df['latitude'].dtype == np.float64
    assert df['longitude'].dtype == np.float64

def test_load_data_with_errors(mock_data_with_errors):
    """Tests data cleaning: coercing errors to NaN and dropping rows."""
    file_path, _ = mock_data_with_errors
    df = load_data_for_mapping(str(file_path))
    
    # Only the first row (48.85, 2.29) is expected to survive the cleaning
    assert len(df) == 1
    assert df['latitude'].iloc[0] == pytest.approx(48.85)
    assert df['longitude'].iloc[0] == pytest.approx(2.29)

def test_load_data_missing_column(tmp_path):
    """Tests if KeyError is raised when a required column is missing."""
    file_path = tmp_path / "missing_col.csv"
    df = pd.DataFrame({'latitude': [1], 'longitude': [1]})
    df.to_csv(file_path, index=False)
    
    with pytest.raises(KeyError, match="Required column 'scientific_name' not found"):
        load_data_for_mapping(str(file_path))

def test_load_data_empty(mock_empty_data):
    """Tests loading an empty file."""
    file_path, _ = mock_empty_data
    df = load_data_for_mapping(str(file_path))
    assert len(df) == 0

# --- Test calculate_auto_extent function ---

def test_calculate_auto_extent_valid(mock_valid_data):
    """Tests extent calculation with padding and boundary clamping."""
    _, df = mock_valid_data
    padding = PLOT_CONFIG['MAP_EXTENT_PADDING']
    lon_min, lon_max, lat_min, lat_max = calculate_auto_extent(df, padding)
    
    # Calculated min lon: -75.0060. Clamped to EU boundary lon_min: -20.0
    # Calculated max lon: 3.2945. (No clamp)
    # Calculated min lat: 39.7128. (No clamp)
    # Calculated max lat: 52.5074. (No clamp)

    assert lon_min == pytest.approx(-20.0)
    assert lon_max == pytest.approx(3.2945)
    assert lat_min == pytest.approx(39.7128)
    assert lat_max == pytest.approx(52.5074)

def test_calculate_auto_extent_empty(mock_empty_data):
    """Tests that default EU extent is returned for empty data."""
    _, df = mock_empty_data
    expected_extent = PLOT_CONFIG['EU_BOUNDARY_EXTENT']
    result_extent = calculate_auto_extent(df, PLOT_CONFIG['MAP_EXTENT_PADDING'])
    assert result_extent == expected_extent


# --- Test plot_observations_on_map function (Requires Mocks) ---

# To avoid circular imports when patching, we need to adjust the patch target
# to the module where it is imported (which is where the test script assumes the function is).
# Based on the user's directory structure, the path should be used in the patch.
# If the test script is run as part of a Pytest collection, the full module path is safer.
MODULE_PATH = 'src.flickr_to_plantnet.generate_observation_map'
# If running the test from the same folder as the script, you might use:
# MODULE_PATH = 'generate_observation_map' 

@patch('matplotlib.pyplot.savefig')
@patch('matplotlib.pyplot.colorbar')
@patch('matplotlib.pyplot.close')
@patch('matplotlib.pyplot.title')
@patch('matplotlib.pyplot.figure')
# NOTE: Removed the failing @patch('cartopy.mpl.geoaxes.GeoAxes.scatter')
def test_plot_observations_on_map_calls_save(mock_figure, mock_title, mock_close, mock_colorbar, mock_savefig, mock_valid_data):
    """Tests that the plotting function runs and calls savefig."""
    file_path, df = mock_valid_data
    output_path = "test_output.png"

    # Mock figure/ax setup
    mock_fig = MagicMock()
    mock_ax = MagicMock()
    
    # FIX: Manually mock the scatter method on the mocked axis object
    mock_scatter = MagicMock() 
    mock_ax.scatter = mock_scatter 
    
    mock_figure.return_value = mock_fig
    mock_fig.add_subplot.return_value = mock_ax

    plot_observations_on_map(df, output_path)

    # Check that scatter was called once using the local mock_scatter
    mock_scatter.assert_called_once()
    
    # Check that savefig and close were called
    mock_savefig.assert_called_once_with(output_path, dpi=300, bbox_inches='tight')
    mock_close.assert_called_once_with(mock_fig)
    
    # Check if the title was set correctly
    expected_species = df['scientific_name'].iloc[0]
    mock_title.assert_called_once_with(f'High-Fidelity Observations of {expected_species} (N={len(df)})', fontsize=16)

@patch('matplotlib.pyplot.savefig')
@patch('matplotlib.pyplot.close')
@patch('matplotlib.pyplot.title')
@patch('matplotlib.pyplot.figure')
# NOTE: Also fix the empty data test by ensuring mock_ax.scatter is a mock
def test_plot_observations_on_map_empty_data(mock_figure, mock_title, mock_close, mock_savefig, mock_empty_data):
    """Tests plotting logic for empty data (should not call scatter)."""
    file_path, df = mock_empty_data
    output_path = "test_empty_output.png"

    # Mock figure/ax setup
    mock_fig = MagicMock()
    mock_ax = MagicMock()
    mock_ax.scatter = MagicMock() # Ensure scatter is mocked
    mock_figure.return_value = mock_fig
    mock_fig.add_subplot.return_value = mock_ax

    plot_observations_on_map(df, output_path)
    
    # Check that scatter was NOT called
    mock_ax.scatter.assert_not_called()
    
    # Check that savefig and close were still called
    mock_savefig.assert_called_once()
    mock_close.assert_called_once()
    
    # Check for the correct title for empty data
    mock_title.assert_called_once_with('No High-Fidelity Observations to Plot', fontsize=16)


# --- Test generate_observation_map function (FIXED) ---

@patch(f'{MODULE_PATH}.plot_observations_on_map')
@patch(f'{MODULE_PATH}.load_data_for_mapping')
def test_generate_observation_map_end_to_end(mock_load_data, mock_plot_map, tmp_path):
    """Tests the main pipeline function."""
    
    # 1. Setup mock data and paths
    input_file_name = "test_input.csv"
    input_file_path = tmp_path / input_file_name
    pd.DataFrame({'latitude': [1], 'longitude': [1], 'scientific_name': ['S'], 'PlantNet_Confidence_Score': [0.5]}).to_csv(input_file_path, index=False)
    
    # Mock the return value of load_data_for_mapping
    mock_df = pd.DataFrame({'latitude': [1], 'longitude': [1], 'scientific_name': ['S'], 'PlantNet_Confidence_Score': [0.5]})
    mock_load_data.return_value = mock_df
    
    # Set expected output file details
    custom_output_name = "custom_map.png"
    expected_output_path = os.path.join(str(tmp_path), custom_output_name)
    
    # 2. Run the function
    returned_path = generate_observation_map(str(input_file_path), output_filename=custom_output_name, auto_zoom=False)
    
    # 3. Assertions
    mock_load_data.assert_called_once_with(str(input_file_path))
    
    # FIX: assert False positionally, not as a keyword argument
    mock_plot_map.assert_called_once_with(mock_df, expected_output_path, False)
    
    assert returned_path == expected_output_path