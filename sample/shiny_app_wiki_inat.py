import sys
import subprocess
import importlib
import threading
from pathlib import Path

# Required libraries
required_libraries = ["shiny", "fastapi", "starlette", "uvicorn", "pandas", "plotly", "kaleido"]

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
import uvicorn

# Paths (ensure these directories exist in the same directory as your script)
map_dir = Path("maps")
plots_dir = Path("species_plots")
inat_map_dir = Path("inat_maps")

# Load data (with error handling)
try:
    species_data = pd.read_csv("species_pageviews_analysis_2025_01.csv")
    species_counts = species_data[species_data["Status"] == "Increasing"].groupby("Scientific Name").size()
    all_species = species_data["Scientific Name"].unique()

    def get_top_species():
        df = pd.read_csv("species_pageviews_analysis_2025_01.csv")
        red_counts = df[df["Status"] == "Increasing"].groupby("Scientific Name").size()
        return red_counts.nlargest(10)

    top_species = get_top_species()
    print(top_species)

    inat_data = pd.read_csv("species_observation_counts_by_country.csv")
    inat_species_list = inat_data["Scientific Name"].unique()

except FileNotFoundError as e:
    print(f"Error: Data file not found: {e}")
    sys.exit(1)
except Exception as e:
    print(f"Error loading data: {e}")
    sys.exit(1)

# FastAPI app (serves static files)
fastapi_app = FastAPI()
fastapi_app.mount("/static", StaticFiles(directory="."), name="static")

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

# Populate top_species_data with species info
top_species_data = []
for species, count in top_species.items():
    color = get_color(count, top_species.max())
    top_species_data.append({
        'species': species,
        'count': count,
        'color': color
    })

# Create custom title:
custom_title = ui.tags.div(
    ui.tags.span(
        "Species observations and pageview changes last month",
        style="font-weight: bold; font-size: 40px;"
    ),
    ui.img(
        src="http://127.0.0.1:8001/static/OneSTOP_final_logo_1.png",
        style="max-width: 400px; padding: 0; margin-top: 0px;"  # Logo below the title with margin for spacing
    ),
    style="text-align: right; display: block; margin: 0; padding: 0;"
)

# Inject custom CSS to style the tab names: increase font size and bold them.
custom_css = ui.tags.style("""
.navbar-nav .nav-link {
    font-size: 30px;
    font-weight: bold;
}
""")

# Shiny UI
app_ui = ui.page_navbar(
    ui.head_content(custom_css),
    ui.nav_panel(
        "iNaturalist",
        ui.input_select("inat_species", "Select an invasive species:", choices=list(inat_species_list), selected=""),
        ui.output_ui("inat_map_display")
    ),
    ui.nav_panel(
        "Wikipedia",
        ui.input_select("wikipedia_species", "Select an invasive species:", choices=list(all_species), selected=""),
        ui.row(
            ui.column(6, ui.output_ui("map_display")),
            ui.column(6, ui.output_ui("linegraph_display"),
                      style="display: flex; align-items: center; justify-content: center;")
        ),
        ui.h2("Top 10 most searched species last month", style="font-weight: bold;"),
        ui.output_ui("top_species_cards")
    ),
    title=custom_title  # Setting the custom title at the end
)

# Shiny Server
def server(input, output, session):

    @output
    @render.ui
    def top_species_cards():
        if top_species_data:
            cards = [
                ui.card(
                    ui.h5(card["species"], style="font-weight: bold;"),
                    ui.p(f"Increased search volume in {card['count']} countries"),
                    style=f"background-color: {card['color']}; padding: 10px; text-align: center; color: black; cursor: pointer; font-size: 12px;",
                    onclick=f"Shiny.setInputValue('wikipedia_species', '{card['species']}');"
                ) for card in top_species_data
            ]
            return ui.layout_column_wrap(*cards, width=1/5)
        else:
            return ui.p("No species meet the criteria for increased search volume.")

    @output
    @render.ui
    def inat_map_display():
        selected_species = input.inat_species()
        if not selected_species:
            return ui.HTML("Select a species to display its map.")
        map_filename = f"{selected_species.replace(' ', '_')}_map.html"
        map_url = f"http://127.0.0.1:8001/static/inat_maps/{map_filename}"
        map_path = inat_map_dir / map_filename
        print(f"Looking for iNaturalist map at: {map_path}")
        if map_path.exists():
            return ui.HTML(f'<h3 style="font-weight: bold;">{selected_species}</h3><iframe src="{map_url}" width="100%" height="600px"></iframe>')
        else:
            return ui.HTML(f"Map not found for {selected_species}.")

    @output
    @render.ui
    def map_display():
        selected_species = input.wikipedia_species()
        if not selected_species:
            return ui.HTML("Select a species to display its map.")
        map_filename = f"{selected_species.replace(' ', '_')}_map.html"
        map_url = f"http://127.0.0.1:8001/static/maps/{map_filename}"
        map_path = map_dir / map_filename
        print(f"Looking for Wikipedia map at: {map_path}")
        if map_path.exists():
            return ui.HTML(f'<h3 style="font-weight: bold;">{selected_species}</h3><iframe src="{map_url}" width="100%" height="600px"></iframe>')
        else:
            return ui.HTML(f"Map not found for {selected_species}.")

    @output
    @render.ui
    def linegraph_display():
        selected_species = input.wikipedia_species()
        if not selected_species:
            return ui.HTML("Select a species to display its line graph.")
        plot_filename = f"{selected_species.replace(' ', '_')}.png"
        plot_url = f"http://127.0.0.1:8001/static/species_plots/{plot_filename}"
        plot_path = plots_dir / plot_filename
        if plot_path.exists():
            return ui.HTML(f'<img src="{plot_url}" width="100%" />')
        else:
            return ui.HTML(f"Line graph not found for {selected_species}.")

app = App(app_ui, server)

if __name__ == "__main__":
    app.run(port=8000)
