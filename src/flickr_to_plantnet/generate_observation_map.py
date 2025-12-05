import pandas as pd
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature  
from typing import Optional, Tuple
import os

# --- Configuration for Plotting ---
PLOT_CONFIG = {
    'OUTPUT_FILE': "high_fidelity_observations_map.png",
    'MAP_EXTENT_PADDING': 1.0, # Degrees of latitude/longitude to pad the auto-zoom
    'EU_BOUNDARY_EXTENT': [-20, 40, 35, 70] # A reasonable default extent for the European region: [lon_min, lon_max, lat_min, lat_max]
}

def load_data_for_mapping(file_path: str) -> pd.DataFrame:
    """
    Load the plotting dataset and ensure coordinates are numeric.
    
    Args:
        file_path: Path to the high-fidelity plotting CSV file.
        
    Returns:
        DataFrame containing observation coordinates.
    """
    print(f"📊 Loading data from: {file_path}")
    df = pd.read_csv(file_path)
    
    # Ensure necessary columns exist and convert coordinates to numeric types
    required_cols = ['latitude', 'longitude', 'scientific_name', 'PlantNet_Confidence_Score']
    for col in required_cols:
        if col not in df.columns:
            # Note: The data you are using (full_API_validated_plants.csv) 
            # might be missing 'scientific_name' if you didn't run the merge step fully. 
            # I am keeping this check, but be aware of the data file you pass!
            raise KeyError(f"Required column '{col}' not found in the dataset.")
            
    df['latitude'] = pd.to_numeric(df['latitude'], errors='coerce')
    df['longitude'] = pd.to_numeric(df['longitude'], errors='coerce')
    
    # Drop any rows where coordinates are now NaN (bad data)
    df.dropna(subset=['latitude', 'longitude'], inplace=True)
    
    print(f"✅ Loaded {len(df)} observations with valid coordinates.")
    return df

def calculate_auto_extent(df: pd.DataFrame, padding: float) -> Tuple[float, float, float, float]:
    """
    Calculates the minimum map extent needed to cover all observations 
    with specified padding.
    """
    if df.empty:
        print("⚠️ No valid data points found. Using default EU extent.")
        return PLOT_CONFIG['EU_BOUNDARY_EXTENT']

    # Calculate min/max coordinates
    lon_min = df['longitude'].min() - padding
    lon_max = df['longitude'].max() + padding
    lat_min = df['latitude'].min() - padding
    lat_max = df['latitude'].max() + padding
    
    # Clamp the calculated extent to a reasonable European boundary
    eu_lon_min, eu_lon_max, eu_lat_min, eu_lat_max = PLOT_CONFIG['EU_BOUNDARY_EXTENT']
    
    lon_min = max(lon_min, eu_lon_min)
    lon_max = min(lon_max, eu_lon_max)
    lat_min = max(lat_min, eu_lat_min)
    lat_max = min(lat_max, eu_lat_max)

    return lon_min, lon_max, lat_min, lat_max

def plot_observations_on_map(df: pd.DataFrame, output_path: str, auto_zoom: bool = True):
    """
    Plots the observation data on a Cartopy map and saves it as a static PNG.
    """
    print("🗺️ Generating map visualization...")
    
    # 1. Setup the plot and projection
    fig = plt.figure(figsize=(12, 12))
    ax = fig.add_subplot(1, 1, 1, projection=ccrs.PlateCarree())
    
    # 2. Configure map features
    ax.coastlines(resolution='50m', color='gray')
    ax.add_feature(cfeature.BORDERS, linestyle=':', alpha=0.5) # <--- FIXED
    ax.add_feature(cfeature.LAND, facecolor='#eeeeee')         # <--- FIXED
    ax.add_feature(cfeature.OCEAN, facecolor='#bfe8f9')        # <--- FIXED
    
    # 3. Determine the map extent (zoom level)
    if auto_zoom and not df.empty:
        lon_min, lon_max, lat_min, lat_max = calculate_auto_extent(df, PLOT_CONFIG['MAP_EXTENT_PADDING'])
        # If all points are the same (like in your example), ensure a minimum zoom
        if lon_max - lon_min < 2: lon_max += 1; lon_min -= 1
        if lat_max - lat_min < 2: lat_max += 1; lat_min -= 1
        print(f"🔍 Auto-set map extent to: Lon {lon_min:.2f}-{lon_max:.2f}, Lat {lat_min:.2f}-{lat_max:.2f}")
    else:
        # Fallback to the default EU boundary if auto-zoom is off or data is empty
        lon_min, lon_max, lat_min, lat_max = PLOT_CONFIG['EU_BOUNDARY_EXTENT']
    
    ax.set_extent([lon_min, lon_max, lat_min, lat_max], crs=ccrs.PlateCarree())
    
    # Add gridlines with labels only on the left/bottom
    gl = ax.gridlines(draw_labels=True, linewidth=0.5, color='gray', alpha=0.5, linestyle='--')
    gl.top_labels = False
    gl.right_labels = False
    
    # 4. Plot the data points
    if not df.empty:
        # Use confidence score to set marker size or color if desired
        sizes = (df['PlantNet_Confidence_Score'] * 100) + 10 # Scale score to size, min size 10
        scatter = ax.scatter(
            df['longitude'], 
            df['latitude'], 
            c=df['PlantNet_Confidence_Score'], # Color by confidence
            s=sizes, 
            edgecolor='k', 
            marker='o', 
            alpha=0.8,
            cmap='viridis', # Use a color map
            transform=ccrs.PlateCarree()
        )
        
        # Add color bar
        cbar = fig.colorbar(scatter, ax=ax, orientation='horizontal', pad=0.05, aspect=50)
        cbar.set_label('PlantNet Confidence Score')
        
        # Add title
        species = df['scientific_name'].iloc[0] if len(df) > 0 else "Observations"
        plt.title(f'High-Fidelity Observations of {species} (N={len(df)})', fontsize=16)
        
    else:
        plt.title('No High-Fidelity Observations to Plot', fontsize=16)

    # 5. Save the figure
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    plt.close(fig)
    print(f"✅ Map saved to: {output_path}")

# ==============================================================================
# Main function for notebook use
# ==============================================================================

def generate_observation_map(data_file_path: str, 
                             output_filename: Optional[str] = None, 
                             auto_zoom: bool = True) -> str:
    """
    Main function to run the mapping pipeline.
    """
    if output_filename:
        PLOT_CONFIG['OUTPUT_FILE'] = output_filename
        
    df = load_data_for_mapping(data_file_path)
    
    # If the input file is simply 'full_API_validated_plants.csv', os.path.dirname returns ''
    # which means the output will be saved in the current working directory.
    output_path = os.path.join(os.path.dirname(data_file_path), PLOT_CONFIG['OUTPUT_FILE'])
    
    plot_observations_on_map(df, output_path, auto_zoom)
    
    return output_path