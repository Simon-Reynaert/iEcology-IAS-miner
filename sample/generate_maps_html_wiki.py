import sys
import subprocess
import importlib

# List of required libraries
required_libraries = ["folium", "pandas", "geopandas","cartopy","shutil","zipfile","requests","os"]

# Function to check and install missing libraries
def install_missing_libraries():
    for lib in required_libraries:
        try:
            importlib.import_module(lib)  # Try importing the module
        except ImportError:
            print(f"{lib} not found. Installing...")
            subprocess.check_call([sys.executable, "-m", "pip", "install", "--upgrade", lib])

# Run the installation check
install_missing_libraries()

# Now, safely import the required libraries
import os
import folium
import pandas as pd
import geopandas as gpd

# Ensure the output directory exists
map_dir = "maps"
if not os.path.exists(map_dir):
    os.makedirs(map_dir)

# Load the species data
df = pd.read_csv("species_pageviews_analysis_2025_01.csv")

# Define language to country mapping (for EU countries)
language_to_country = {
    "de": "Germany", "fr": "France", "es": "Spain", "it": "Italy", "nl": "Netherlands",
    "pl": "Poland", "sv": "Sweden", "da": "Denmark", "fi": "Finland", "no": "Norway",
    "pt": "Portugal", "cs": "Czech Republic", "sk": "Slovakia", "hu": "Hungary",
    "ro": "Romania", "bg": "Bulgaria", "el": "Greece", "lt": "Lithuania", "lv": "Latvia",
    "et": "Estonia", "sl": "Slovenia", "hr": "Croatia", "mt": "Malta", "lb": "Luxembourg",
    "en": "Ireland", "ga": "Ireland", "cy": "Cyprus"
}

import os
import requests
import zipfile
import geopandas as gpd

# Define directory and file names
shapefile_dir = "natural_earth"
shapefile_name = "ne_110m_admin_0_countries"
zip_path = os.path.join(shapefile_dir, f"{shapefile_name}.zip")
shapefile_path = os.path.join(shapefile_dir, f"{shapefile_name}.shp")

# Ensure the directory exists
os.makedirs(shapefile_dir, exist_ok=True)

# URL for the Natural Earth dataset
url = f"https://naciscdn.org/naturalearth/110m/cultural/{shapefile_name}.zip"

# Step 1: Download the dataset if it doesn't exist
if not os.path.exists(shapefile_path):
    print("Downloading Natural Earth dataset...")
    
    response = requests.get(url, stream=True)
    
    if response.status_code == 200:
        with open(zip_path, "wb") as file:
            file.write(response.content)
        print("Download complete.")
        
        # Step 2: Extract the ZIP file
        with zipfile.ZipFile(zip_path, "r") as zip_ref:
            zip_ref.extractall(shapefile_dir)
        print("Extraction complete.")
        
        # Step 3: Remove ZIP file after extraction
        os.remove(zip_path)
    else:
        print("Failed to download the file. Check the URL or your internet connection.")

# Step 4: Verify that all necessary files exist
required_extensions = [".shp", ".shx", ".dbf", ".prj"]
missing_files = [ext for ext in required_extensions if not os.path.exists(os.path.join(shapefile_dir, shapefile_name + ext))]

if missing_files:
    raise FileNotFoundError(f"Missing required shapefile components: {missing_files}")

# Step 5: Load the shapefile
world = gpd.read_file(shapefile_path)

print("Shapefile loaded successfully!")

# Filter out only the countries in Europe
europe = world[world["CONTINENT"] == "Europe"]

# Function to generate interactive folium map for species
def generate_folium_map(species_df, language_to_country):
    # Initialize map centered around Europe
    m = folium.Map(location=[50, 10], zoom_start=4)

    # Assign color to countries based on the species' status
    for _, row in species_df.iterrows():
        country = language_to_country.get(row["Language"])
        if country:
            status = row["Status"]
            color = "red" if status == "Increasing" else "gray"
            # Plot country on map with status
            country_geom = europe[europe["NAME"] == country]
            if not country_geom.empty:
                folium.GeoJson(
                    country_geom.geometry,
                    style_function=lambda x, color=color: {
                        'fillColor': color, 
                        'color': 'black', 
                        'weight': 0.5, 
                        'fillOpacity': 0.7
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
        <div style="display: flex; align-items: center; margin-bottom: 5px;">
            <span style="width: 20px; height: 20px; background-color: red; border: 1px solid #000; display: inline-block; margin-right: 8px;"></span>
            Increase in wiki pageviews last month
        </div>
        <div style="display: flex; align-items: center;">
            <span style="width: 20px; height: 20px; background-color: gray; border: 1px solid #000; display: inline-block; margin-right: 8px;"></span>
            No change in wiki pageviews last month
        </div>
    </div>
    '''

    m.get_root().html.add_child(folium.Element(legend_html))

    # Add zoom controls
    folium.LayerControl().add_to(m)
    return m

# Generate maps for each species and save them
species_list = df["Scientific Name"].unique()
for species in species_list:
    species_df = df[df["Scientific Name"] == species]

    # Generate the folium map
    folium_map = generate_folium_map(species_df, language_to_country)

    # Save map as HTML
    map_html_path = os.path.join(map_dir, f"{species.replace(' ', '_')}_map.html")
    print(f"Saving map for {species} at {map_html_path}")
    folium_map.save(map_html_path)

print("All maps have been generated successfully!")
