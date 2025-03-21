import sys
import subprocess
import importlib
import threading
from pathlib import Path

# Required libraries
required_libraries = ["shiny", "fastapi", "starlette", "uvicorn", "pandas"]

def install_missing_libraries():
    for lib in required_libraries:
        try:
            importlib.import_module(lib)
        except ImportError:
            print(f"{lib} not found. Installing...")
            subprocess.check_call([sys.executable, "-m", "pip", "install", "--upgrade", lib])

install_missing_libraries()

import pandas as pd
from shiny import App, ui, render
from fastapi import FastAPI
from starlette.staticfiles import StaticFiles
from starlette.requests import Request
import uvicorn

# Paths (ensure these directories exist in the same directory as your script)
map_dir = Path("maps_alert_wiki_2022_present") #wiki data
inat_map_dir = Path("maps_alert_inat_2022_present") #inat data
flickr_map_dir = Path("flickr_maps") #flickr data

# Load alert data (with error handling)
try:
    # Use species_alert_info_wiki.csv to determine the species with TRUE alerts for Wikipedia
    alert_data_wiki = pd.read_csv("species_alert_info_wiki.csv")
    # Count only TRUE alerts (assuming the 'alert' column is boolean or "TRUE"/"FALSE")
    true_counts_wiki = alert_data_wiki[alert_data_wiki["alert"] == True].groupby("species").size()
    all_species = alert_data_wiki["species"].unique()

    def get_top_species_wiki():
        return true_counts_wiki.nlargest(10)

    top_species_wiki = get_top_species_wiki()
    print(top_species_wiki)

    # Use species_alert_info_inat.csv to determine the species with TRUE alerts for iNaturalist
    alert_data_inat = pd.read_csv("species_alert_info_inat.csv")
    true_counts_inat = alert_data_inat[alert_data_inat["alert"] == True].groupby("species").size()
    inat_species_list = alert_data_inat["species"].unique()

    def get_top_species_inat():
        return true_counts_inat.nlargest(10)
        
    top_species_inat = get_top_species_inat()
    print(top_species_inat)

except FileNotFoundError as e:
    print(f"Error: Data file not found: {e}")
    sys.exit(1)
except Exception as e:
    print(f"Error loading data: {e}")
    sys.exit(1)

# FastAPI app (serves static files without caching)
class CustomStaticFiles(StaticFiles): #set https header to not cache any data
    async def get_response(self, path: str, request: Request):
        response = await super().get_response(path, request)
        response.headers["Cache-Control"] = "no-cache, no-store, must-revalidate"
        response.headers["Pragma"] = "no-cache"
        response.headers["Expires"] = "0"
        return response

fastapi_app = FastAPI()
fastapi_app.mount("/static", StaticFiles(directory=".", html=True), name="static")

def run_fastapi():
    uvicorn.run(fastapi_app, host="127.0.0.1", port=8001)

threading.Thread(target=run_fastapi, daemon=True).start()

# Color gradient function remains unchanged
def get_color(value, max_value):
    steps = 10
    ratio = value / max_value if max_value > 0 else 0
    step_index = int(ratio * (steps - 1))
    colors = [(255, 0, 0), (255, 51, 0), (255, 102, 0), (255, 153, 0), (255, 204, 0),
              (255, 255, 0), (204, 255, 0), (153, 255, 0), (102, 255, 0), (51, 255, 0)]
    red, green, blue = colors[step_index]
    return f"rgb({green}, {red}, {blue})"

# Process top species data for Wikipedia alerts
top_species_data_wiki = []
if not true_counts_wiki.empty:
    top_species_sorted_wiki = true_counts_wiki.nlargest(10).reset_index()
    for _, row in top_species_sorted_wiki.iterrows():
        species = row["species"]
        count = row[0]
        color = get_color(count, top_species_sorted_wiki.iloc[0, 1])  # Normalize to the max count
        top_species_data_wiki.append({
            'species': species,
            'count': count,
            'color': color
        })

# Process top species data for iNaturalist alerts
top_species_data_inat = []
if not true_counts_inat.empty:
    top_species_sorted_inat = true_counts_inat.nlargest(10).reset_index()
    for _, row in top_species_sorted_inat.iterrows():
        species = row["species"]
        count = row[0]
        color = get_color(count, top_species_sorted_inat.iloc[0, 1])  # Normalize to the max count
        top_species_data_inat.append({
            'species': species,
            'count': count,
            'color': color
        })

# Create custom title with space between the title and logo horizontally
custom_title = ui.tags.div(
    ui.tags.span(
        "Species observations and alert changes per platform",
        style="font-weight: bold; font-size: 30px; margin-right: 100px;"
    ),
    ui.img(
        src="http://127.0.0.1:8001/static/OneSTOP_final_logo_1.png",
        style="max-width: 300px; padding: 0; margin-top: 0px;"
    ),
    style="text-align: right; display: flex; align-items: center; justify-content: flex-end; margin: 0px; padding: 0px;"
)

# Inject custom CSS to style the tab names
custom_css = ui.tags.style("""
.navbar-nav .nav-link {
    font-size: 20px;
    font-weight: bold;
}
""")

# Disclaimer message for Wikipedia tab
disclaimer_message = ui.tags.div(
    ui.tags.p(
        "DISCLAIMER: Wikipedia per country statistics are obtained through language-based filtering. For more info about accuracy: ",
        ui.tags.a("Wikipedia Statistics", href="https://stats.wikimedia.org/wikimedia/squids/SquidReportPageViewsPerLanguageBreakdown.htm", target="_blank"),
        style="color: red; font-size: 16px; font-weight: bold; margin-bottom: 20px;"
    )
)

# Shiny UI with the updated Wikipedia and iNaturalist tabs
app_ui = ui.tags.div(
    custom_title,
    ui.page_navbar(
        ui.head_content(custom_css),
        ui.nav_panel(
            "iNaturalist",
            ui.input_select("inat_species", "Select an invasive species:", choices=list(inat_species_list), selected=""),
            ui.output_ui("inat_map_display"),
            ui.h2("Top 10 species with increased iNaturalist alerts", style="font-weight: bold; font-size: 30px"),
            ui.output_ui("top_species_cards_inat")
        ),
        ui.nav_panel(
            "Wikipedia",
            disclaimer_message,  # Add the disclaimer message here
            ui.input_select("wikipedia_species", "Select an invasive species:", choices=list(all_species), selected=all_species[0] if len(all_species) > 0 else ""),
            ui.output_ui("wiki_map_display"),
            ui.h2("Top 10 species with increased alerts", style="font-weight: bold; font-size: 30px"),
            ui.output_ui("top_species_cards_wiki")
        ),
        ui.nav_panel(
            "Flickr",
            ui.input_select("flickr_species", "Select a species:", choices=list(all_species), selected=""),
            ui.output_ui("flickr_map_display")
        )
    ),
    style="max-width: 1300px; margin: 0 auto; padding: 30px;"
)

# Shiny Server
def server(input, output, session):

    # Wiki tiles
    @output
    @render.ui
    def top_species_cards_wiki():
        if top_species_data_wiki:
            sorted_species_data = sorted(top_species_data_wiki, key=lambda x: x["count"], reverse=True)

            cards = [
                ui.card(
                    ui.h5(card["species"], style="font-weight: bold;"),
                    ui.p(f"Alert in {card['count']} languages"),
                    style=f"background-color: {card['color']}; padding: 10px; text-align: center; color: black; cursor: pointer; font-size: 15px; border-radius: 8px;",
                    onclick=f"Shiny.setInputValue('wikipedia_species', '{card['species']}');"
                ) for card in sorted_species_data
            ]
            
            return ui.layout_column_wrap(*cards, width=1/5)
        else:
            return ui.p("No species meet the criteria for Wikipedia increased alerts.")

    # iNaturalist tiles
    @output
    @render.ui
    def top_species_cards_inat():
        if top_species_data_inat:
            sorted_species_data = sorted(top_species_data_inat, key=lambda x: x["count"], reverse=True)

            cards = [
                ui.card(
                    ui.h5(card["species"], style="font-weight: bold;"),
                    ui.p(f"Alert in {card['count']} countries"),
                    style=f"background-color: {card['color']}; padding: 10px; text-align: center; color: black; cursor: pointer; font-size: 15px; border-radius: 8px;",
                    onclick=f"Shiny.setInputValue('inat_species', '{card['species']}');"
                ) for card in sorted_species_data
            ]
            
            return ui.layout_column_wrap(*cards, width=1/5)
        else:
            return ui.p("No species meet the criteria for increased iNaturalist alerts.")

    @output
    @render.ui
    def inat_map_display():
        selected_species = input.inat_species()
        if not selected_species:
            return ui.HTML("Select a species to display its map.")
        map_filename = f"{selected_species.replace(' ', '_')}_map.html"
        map_url = f"http://127.0.0.1:8001/static/maps_alert_inat_2022_present/{map_filename}"
        map_path = inat_map_dir / map_filename
        print(f"Looking for iNaturalist map at: {map_path}")
        if map_path.exists():
            return ui.HTML(f'<h3 style="font-weight: bold;">{selected_species}</h3><iframe src="{map_url}" width="100%" height="600px"></iframe>')
        else:
            return ui.HTML(f"Map not found for {selected_species}.")

    @output
    @render.ui
    def wiki_map_display():
        selected_species = input.wikipedia_species()
        if not selected_species:
            return ui.HTML("Select a species to display its map.")
        map_filename = f"{selected_species.replace(' ', '_')}_map.html"
        map_url = f"http://127.0.0.1:8001/static/maps_alert_wiki_2022_present/{map_filename}"
        map_path = map_dir / map_filename
        print(f"Looking for Wikipedia map at: {map_path}")
        if map_path.exists():
            return ui.HTML(f'<h3 style="font-weight: bold;">{selected_species}</h3><iframe src="{map_url}" width="100%" height="600px"></iframe>')
        else:
            return ui.HTML(f"Map not found for {selected_species}.")

    @output
    @render.ui
    def flickr_map_display():
        selected_species = input.flickr_species()
        if not selected_species:
            return ui.HTML("Select a species to display its Flickr map.")
        map_filename = f"{selected_species.replace(' ', '_')}_map.html"
        map_url = f"http://127.0.0.1:8001/static/flickr_maps/{map_filename}"
        map_path = flickr_map_dir / map_filename
        print(f"Looking for Flickr map at: {map_path}")
        if map_path.exists():
            return ui.HTML(f'<h3 style="font-weight: bold;">{selected_species}</h3><iframe src="{map_url}" width="100%" height="600px"></iframe>')
        else:
            return ui.HTML(f"Map not found for {selected_species}.")

app = App(app_ui, server)

if __name__ == "__main__":
    app.run(port=8000)
