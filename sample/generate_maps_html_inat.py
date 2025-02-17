import sys
import subprocess
import importlib
import os
import folium
import pandas as pd
import geopandas as gpd
import requests
import zipfile
from thefuzz import process

# List of required libraries
required_libraries = ["folium", "pandas", "geopandas", "requests", "zipfile", "thefuzz"]

# Function to check and install missing libraries
def install_missing_libraries():
    for lib in required_libraries:
        try:
            importlib.import_module(lib)
        except ImportError:
            print(f"{lib} not found. Installing...")
            subprocess.check_call([sys.executable, "-m", "pip", "install", "--upgrade", lib])

# Run the installation check
install_missing_libraries()

# Ensure the output directory exists
map_dir = "inat_maps"
os.makedirs(map_dir, exist_ok=True)

# Load species data
df = pd.read_csv("species_observation_counts_by_country.csv")

# Download and load Natural Earth dataset
shapefile_dir = "natural_earth"
shapefile_name = "ne_110m_admin_0_countries"
zip_path = os.path.join(shapefile_dir, f"{shapefile_name}.zip")
shapefile_path = os.path.join(shapefile_dir, f"{shapefile_name}.shp")

os.makedirs(shapefile_dir, exist_ok=True)
url = f"https://naciscdn.org/naturalearth/110m/cultural/{shapefile_name}.zip"

if not os.path.exists(shapefile_path):
    response = requests.get(url, stream=True)
    if response.status_code == 200:
        with open(zip_path, "wb") as file:
            file.write(response.content)
        with zipfile.ZipFile(zip_path, "r") as zip_ref:
            zip_ref.extractall(shapefile_dir)
        os.remove(zip_path)

world = gpd.read_file(shapefile_path)
europe = world[world["CONTINENT"] == "Europe"]

# Create a mapping of best-matched country names
country_names = europe["NAME"].tolist()
df["matched_country"] = df["country"].apply(lambda x: process.extractOne(x, country_names)[0])

def calculate_alpha(obs_count):
    if obs_count == 0:
        return 0.7, "gray"  # Grey color with alpha 0.7 for zero observations
    elif obs_count <= 5:
        return 0.40, "rgba(255, 0, 0, 0.40)"  # Red with low alpha for low observations
    elif obs_count <= 20:
        return 0.60, "rgba(255, 0, 0, 0.60)"  # Red with medium alpha for mid-range observations
    elif obs_count <= 40:
        return 0.80, "rgba(255, 0, 0, 0.80)"  # Red with high alpha for higher observations
    else:
        return 1.0, "rgba(255, 0, 0, 1)"  # Fully opaque red for very high observations

def generate_folium_map(species_df):
    m = folium.Map(location=[50, 10], zoom_start=4, control_scale=True)
    
    for _, country_row in europe.iterrows():
        country_name = country_row["NAME"]
        country_geom = country_row.geometry
        country_data = species_df[species_df["matched_country"] == country_name]
        
        if not country_data.empty:
            obs_count = country_data["observation_count"].values[0]
            alpha, color = calculate_alpha(obs_count)
        else:
            alpha, color = calculate_alpha(0)  # For zero observations
        
        folium.GeoJson(
            country_geom,
            style_function=lambda x, color=color, alpha=alpha: {
                'fillColor': color,
                'color': 'black',
                'weight': 0.5,
                'fillOpacity': alpha
            }
        ).add_to(m)
    
    legend_html = '''
    <div style="
        position: fixed;
        bottom: 10px;
        left: 10px;
        background: white;
        padding: 10px;
        border-radius: 5px;
        font-family: Arial, sans-serif;
        font-size: 14px;
        box-shadow: 2px 2px 5px rgba(0, 0, 0, 0.3);
        z-index: 9999;
    ">
        <div style="margin-bottom: 5px; font-weight: bold;">Observation Count Legend:</div>
        <div style="display: flex; align-items: center; margin-bottom: 5px;">
            <span style="width: 20px; height: 20px; background-color: rgba(128, 128, 128, 0.7); border: 1px solid #000; display: inline-block; margin-right: 8px;"></span>
            0 observations
        </div>
        <div style="display: flex; align-items: center; margin-bottom: 5px;">
            <span style="width: 20px; height: 20px; background-color: rgba(255, 0, 0, 0.4); border: 1px solid #000; display: inline-block; margin-right: 8px;"></span>
            1-5 observations
        </div>
        <div style="display: flex; align-items: center; margin-bottom: 5px;">
            <span style="width: 20px; height: 20px; background-color: rgba(255, 0, 0, 0.6); border: 1px solid #000; display: inline-block; margin-right: 8px;"></span>
            6-20 observations
        </div>
        <div style="display: flex; align-items: center; margin-bottom: 5px;">
            <span style="width: 20px; height: 20px; background-color: rgba(255, 0, 0, 0.8); border: 1px solid #000; display: inline-block; margin-right: 8px;"></span>
            21-40 observations
        </div>
        <div style="display: flex; align-items: center;">
            <span style="width: 20px; height: 20px; background-color: rgba(255, 0, 0, 1); border: 1px solid #000; display: inline-block; margin-right: 8px;"></span>
            >40 observations
        </div>
    </div>
    '''
    
    m.get_root().html.add_child(folium.Element(legend_html))
    folium.LayerControl().add_to(m)
    return m

species_list = df["Scientific Name"].unique()
for species in species_list:
    species_df = df[df["Scientific Name"] == species]
    folium_map = generate_folium_map(species_df)
    map_html_path = os.path.join(map_dir, f"{species.replace(' ', '_')}_map.html")
    folium_map.save(map_html_path)
    print(f"Saved map for {species} at {map_html_path}")

print("All maps have been generated successfully!")
