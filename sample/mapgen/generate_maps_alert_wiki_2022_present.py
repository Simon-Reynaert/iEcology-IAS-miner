import sys
import subprocess
import importlib

# List of required libraries
required_libraries = ["folium", "pandas", "geopandas", "cartopy", "shutil", "zipfile", "requests", "os"]

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
import requests
import zipfile
import base64

# Ensure the output directory exists
map_dir = "maps_alert_wiki_2022_present"
os.makedirs(map_dir, exist_ok=True)

# Load the species alert data
df = pd.read_csv("species_alert_info_wiki.csv")

# Define language to country mapping (for EU countries)
language_to_country = {
    "de": "Germany", "fr": "France", "es": "Spain", "it": "Italy", "nl": "Netherlands",
    "pl": "Poland", "sv": "Sweden", "da": "Denmark", "fi": "Finland", "no": "Norway",
    "pt": "Portugal", "cs": "Czech Republic", "sk": "Slovakia", "hu": "Hungary",
    "ro": "Romania", "bg": "Bulgaria", "el": "Greece", "lt": "Lithuania", "lv": "Latvia",
    "et": "Estonia", "sl": "Slovenia", "hr": "Croatia", "mt": "Malta", "lb": "Luxembourg",
    "en": "Ireland", "ga": "Ireland", "cy": "Cyprus"
}

# Set up paths for the Natural Earth shapefile
shapefile_dir = "natural_earth"
shapefile_name = "ne_110m_admin_0_countries"
zip_path = os.path.join(shapefile_dir, f"{shapefile_name}.zip")
shapefile_path = os.path.join(shapefile_dir, f"{shapefile_name}.shp")

# Ensure the directory exists
os.makedirs(shapefile_dir, exist_ok=True)

# URL for the Natural Earth dataset
url = f"https://naciscdn.org/naturalearth/110m/cultural/{shapefile_name}.zip"

# Download and extract the shapefile if needed
if not os.path.exists(shapefile_path):
    print("Downloading Natural Earth dataset...")
    response = requests.get(url, stream=True)

    if response.status_code == 200:
        with open(zip_path, "wb") as file:
            file.write(response.content)
        print("Download complete.")

        # Extract ZIP file
        with zipfile.ZipFile(zip_path, "r") as zip_ref:
            zip_ref.extractall(shapefile_dir)
        print("Extraction complete.")
        
        os.remove(zip_path)  # Clean up zip file
    else:
        print("Failed to download the file. Check the URL or your internet connection.")

# Check for missing shapefile components
required_extensions = [".shp", ".shx", ".dbf", ".prj"]
missing_files = [ext for ext in required_extensions if not os.path.exists(os.path.join(shapefile_dir, shapefile_name + ext))]

if missing_files:
    raise FileNotFoundError(f"Missing required shapefile components: {missing_files}")

# Load the world shapefile and filter for Europe
world = gpd.read_file(shapefile_path)
europe = world[world["CONTINENT"] == "Europe"]

# Directory containing species plot images
species_plots_folder = "species_plots_wiki"

# Function to generate interactive folium map for species
def generate_folium_map(species_df, language_to_country):
    # Initialize map centered around Europe
    m = folium.Map(location=[50, 10], zoom_start=4)

    # Assign color and create pop-ups with images
    for _, row in species_df.iterrows():
        country = language_to_country.get(row["language"])
        if country:
            alert = row["alert"]
            color = "red" if alert else "gray"

            # Get country geometry
            country_geom = europe[europe["NAME"] == country]
            if not country_geom.empty:
                # Generate image filename (e.g., "Acacia saligna_de.png")
                img_filename = f"{row['species']}_{row['language']}.png"
                img_path = os.path.join(species_plots_folder, img_filename)

                # Check if image exists and encode it in base64
                if os.path.exists(img_path):
                    with open(img_path, "rb") as f:
                        encoded_img = base64.b64encode(f.read()).decode('utf-8')
                    img_html = f'<img src="data:image/png;base64,{encoded_img}" style="width:100%; height:auto;">'
                else:
                    img_html = "No image available"

                # Create pop-up content
                popup_html = f"{img_html}"

                # Create an IFrame with the desired width and height
                iframe = folium.IFrame(html=popup_html, width=720, height=540)
                popup = folium.Popup(iframe, max_width=720,max_height=540)
                # Add country region with pop-up and tooltip
                folium.GeoJson(
                    country_geom.geometry,
                    style_function=lambda x, color=color: {
                        'fillColor': color, 
                        'color': 'black', 
                        'weight': 0.5, 
                        'fillOpacity': 0.7
                    },
                    tooltip=f"{row['species']} - {country}",
                    popup=popup
                ).add_to(m)

    # Add a legend
    legend_html = '''
    <div style="position: fixed; bottom: 10px; left: 10px; background: white; padding: 10px;
                border-radius: 5px; font-family: Arial, sans-serif; font-size: 14px;
                box-shadow: 2px 2px 5px rgba(0, 0, 0, 0.3); z-index: 9999;">
        <div style="display: flex; align-items: center; margin-bottom: 5px;">
            <span style="width: 20px; height: 20px; background-color: red; border: 1px solid #000; display: inline-block; margin-right: 8px;"></span>
            Recent increase in species pageviews
        </div>
        <div style="display: flex; align-items: center;">
            <span style="width: 20px; height: 20px; background-color: gray; border: 1px solid #000; display: inline-block; margin-right: 8px;"></span>
            No recent change in species pageviews
        </div>
    </div>
    '''
    m.get_root().html.add_child(folium.Element(legend_html))

    return m

# Generate maps for each species and save them
species_list = df["species"].unique()
for species in species_list:
    species_df = df[df["species"] == species]

    # Generate the folium map
    folium_map = generate_folium_map(species_df, language_to_country)

    # Save map as HTML inside the 'maps' folder
    map_html_path = os.path.join(map_dir, f"{species.replace(' ', '_')}_map.html")
    print(f"Saving map for {species} at {map_html_path}")
    folium_map.save(map_html_path)

print("All maps have been generated successfully!")
