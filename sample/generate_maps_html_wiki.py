import os
import folium
import pandas as pd
import geopandas as gpd

# Ensure the output directory exists
map_dir = "maps"
if not os.path.exists(map_dir):
    os.makedirs(map_dir)

# Load the species data
df = pd.read_csv("species_pageviews_analysis_2024_12.csv")

# Define language to country mapping (for EU countries)
language_to_country = {
    "de": "Germany", "fr": "France", "es": "Spain", "it": "Italy", "nl": "Netherlands",
    "pl": "Poland", "sv": "Sweden", "da": "Denmark", "fi": "Finland", "no": "Norway",
    "pt": "Portugal", "cs": "Czech Republic", "sk": "Slovakia", "hu": "Hungary",
    "ro": "Romania", "bg": "Bulgaria", "el": "Greece", "lt": "Lithuania", "lv": "Latvia",
    "et": "Estonia", "sl": "Slovenia", "hr": "Croatia", "mt": "Malta", "lb": "Luxembourg",
    "en": "Ireland", "ga": "Ireland", "cy": "Cyprus"
}

# Load the Natural Earth shapefile for Europe (update the path to where you store it)
shapefile_path = "natural_earth/ne_110m_admin_0_countries.shp"
world = gpd.read_file(shapefile_path)

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
