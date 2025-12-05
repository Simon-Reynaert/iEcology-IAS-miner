import os
import requests
import zipfile
import pandas as pd
import geopandas as gpd
import folium

def generate_species_maps(csv_file, shapefile_dir, map_output_dir):
    """
    Generates interactive HTML maps showing the presence of species across European countries.

    The function reads a CSV file containing species presence data by country, 
    downloads and prepares a Natural Earth shapefile (if not already available), 
    and creates individual folium-based HTML maps for each species, highlighting 
    countries in red (present) or gray (absent).

    Args:
        csv_file (str): Path to the input CSV file containing species presence data.
            Must include columns: 'scientific_name' (or 'species'), 'country', and 'present'.
        shapefile_dir (str): Directory for storing or locating the Natural Earth shapefile.
        map_output_dir (str): Directory where the generated HTML maps will be saved.

    Returns:
        list[str]: A list of file paths to the generated HTML maps.

    Side Effects:
        - Downloads and extracts the Natural Earth shapefile if not already present.
        - Creates directories if they do not exist.
        - Saves one HTML map per species in `map_output_dir`.
    """
    shapefile_name = "ne_110m_admin_0_countries"
    zip_path = os.path.join(shapefile_dir, f"{shapefile_name}.zip")
    shapefile_path = os.path.join(shapefile_dir, f"{shapefile_name}.shp")

    # Ensure directories exist
    os.makedirs(shapefile_dir, exist_ok=True)
    os.makedirs(map_output_dir, exist_ok=True)

    # Download and extract Natural Earth data if not already present
    url = f"https://naciscdn.org/naturalearth/110m/cultural/{shapefile_name}.zip"
    if not os.path.exists(shapefile_path):
        print("Downloading Natural Earth shapefile...")
        try:
            response = requests.get(url, stream=True)
            response.raise_for_status()
            with open(zip_path, "wb") as f:
                f.write(response.content)
            with zipfile.ZipFile(zip_path, "r") as zip_ref:
                zip_ref.extractall(shapefile_dir)
            os.remove(zip_path)
            print("Shapefile downloaded and extracted.")
        except requests.exceptions.RequestException:
            print("Failed to download Natural Earth shapefile")
            return []  # Return empty list if download fails

    # Load shapefile and filter for Europe
    world = gpd.read_file(shapefile_path)
    europe = world[world["CONTINENT"] == "Europe"]

    # Load species presence data
    df = pd.read_csv(csv_file)
    df.columns = [col.strip().lower() for col in df.columns]
    df.rename(columns={"scientific_name": "species"}, inplace=True)
    df["country"] = df["country"].str.strip().str.upper().replace({"UK": "GB", "EL": "GR"})

    # Function to generate folium map for one species
    def generate_folium_map(species_name, species_df):
        m = folium.Map(location=[54, 15], zoom_start=4)
        merged = europe.merge(species_df, how="left", left_on="ISO_A2_EH", right_on="country")

        for _, row in merged.iterrows():
            present = row["present"]
            if pd.isna(present):
                continue

            color = "red" if str(present).strip().lower() == "yes" else "gray"
            country_geom = row["geometry"]
            if country_geom:
                folium.GeoJson(
                    country_geom,
                    style_function=lambda x, color=color: {
                        "fillColor": color,
                        "color": "black",
                        "weight": 0.5,
                        "fillOpacity": 0.7
                    },
                    tooltip=f"{species_name} – {row['NAME']}"
                ).add_to(m)

        # Add legend
        legend_html = '''
        <div style="position: fixed; bottom: 10px; left: 10px; background: white; padding: 10px;
                    border-radius: 5px; font-family: Arial, sans-serif; font-size: 14px;
                    box-shadow: 2px 2px 5px rgba(0, 0, 0, 0.3); z-index: 9999;">
            <div style="display: flex; align-items: center; margin-bottom: 5px;">
                <span style="width: 20px; height: 20px; background-color: red; border: 1px solid #000; display: inline-block; margin-right: 8px;"></span>
                Present
            </div>
            <div style="display: flex; align-items: center;">
                <span style="width: 20px; height: 20px; background-color: gray; border: 1px solid #000; display: inline-block; margin-right: 8px;"></span>
                Absent
            </div>
        </div>
        '''
        m.get_root().html.add_child(folium.Element(legend_html))
        return m

    # Generate maps for all species
    generated_maps = []
    species_list = df["species"].unique()
    for species in species_list:
        species_df = df[df["species"] == species]
        folium_map = generate_folium_map(species, species_df)
        map_filename = os.path.join(map_output_dir, f"{species.replace(' ', '_')}_map.html")
        print(f"Saving map for {species} → {map_filename}")
        folium_map.save(map_filename)
        generated_maps.append(map_filename)

    print("✅ All maps generated successfully.")
    return generated_maps


# Main script execution
if __name__ == "__main__":
    csv_file = "species_by_country_presence_EASIN_updated.csv"
    shapefile_dir = "natural_earth"
    map_output_dir = "IAS_presence_easin_maps"

    generate_species_maps(csv_file, shapefile_dir, map_output_dir)
