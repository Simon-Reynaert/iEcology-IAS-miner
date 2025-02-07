import sys
import subprocess
import importlib
import os
import threading
from pathlib import Path

# List of required libraries
required_libraries = ["shiny", "fastapi", "starlette", "uvicorn", "pandas", "plotly", "kaleido"]

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
import pandas as pd
import plotly.express as px
from shiny import App, ui, render
from fastapi import FastAPI
from starlette.staticfiles import StaticFiles
import uvicorn

# Paths
map_dir = Path("maps")
plots_dir = Path("species_plots")

# Load species data to display all species
species_data = pd.read_csv("species_pageviews_analysis_2025_01.csv")
species_counts = species_data[species_data["Status"] == "Increasing"].groupby("Scientific Name").size()
all_species = species_data["Scientific Name"].unique()

# Get top 10 species with most affected countries
def get_top_species():
    df = pd.read_csv("species_pageviews_analysis_2025_01.csv")
    red_counts = df[df["Status"] == "Increasing"].groupby("Scientific Name").size()
    top_species = red_counts.nlargest(10)
    return top_species

top_species = get_top_species()

print(top_species)

# FastAPI app to serve maps and pre-made line graphs
fastapi_app = FastAPI()

# Mount maps and linegraphs under different paths
fastapi_app.mount("/maps", StaticFiles(directory=str(map_dir)), name="maps")
fastapi_app.mount("/species_plots", StaticFiles(directory=str(plots_dir)), name="species_plots")

def run_fastapi():
    uvicorn.run(fastapi_app, host="127.0.0.1", port=8001)

threading.Thread(target=run_fastapi, daemon=True).start()

# Function to generate a color from a 10-step gradient (red to yellow)
def get_color(value, max_value):
    steps = 10  # Number of steps in the gradient
    ratio = value / max_value if max_value > 0 else 0
    step_index = int(ratio * (steps - 1))  # Scale the ratio to the number of steps
    colors = [
        (255, 0, 0),   # Red
        (255, 51, 0),  # Red-Orange
        (255, 102, 0), # Orange
        (255, 153, 0), # Orange-Yellow
        (255, 204, 0), # Yellow-Orange
        (255, 255, 0), # Yellow
        (204, 255, 0), # Light Yellow-Green
        (153, 255, 0), # Light Green
        (102, 255, 0), # Green-Yellow
        (51, 255, 0),  # Yellow-Green
    ]
    
    red, green, blue = colors[step_index]
    return f"rgb({green}, {red}, {blue})"

# UI Layout with line graph on the right and map on the left
app_ui = ui.page_fluid(
    ui.h2("Changes in invasive species searches on Wikipedia last month", style="font-weight: bold;"),
    
    ui.input_select("species", "Select an invasive species:", list(all_species), selected=""),

    ui.row(  # Corrected layout for row
        ui.column( 6,  # Left side - Map Display
            ui.output_ui("map_display"),
            ui.output_text("error_message")
        ),
        ui.column( 6,  # Right side - Line Graph
             ui.div(
                ui.output_ui("linegraph_display"), #display the linegraph
                style="display: flex; align-items: center; height: 100%; justify-content: center;"
            )
        )
    ),
    ui.h2("Top 10 most searched species last month", style="font-weight: bold;"),
    ui.layout_column_wrap(
        width=1/5,
        *[
            ui.card(
                ui.h5(species, style="font-weight: bold;"),
                ui.p(f"Increased search volume in {species_counts.get(species, 0)} countries"),
                style=f"background-color: {get_color(species_counts.get(species, 0), species_counts.max())}; padding: 10px; text-align: center; color: black; cursor: pointer; font-size: 12px;",
                onclick=f"Shiny.setInputValue('species', '{species}'); Shiny.setInputValue('dropdown_species', '{species}');" # Set the selected species in the dropdown
            )
            for species in top_species.index
        ]
    )
)




# Server Logic
def server(input, output, session):
    @output
    @render.ui
    def map_display():
        selected_species = input.species()
        if not selected_species:
            return ui.HTML("Select a species to display its map.")
        map_filename = f"{selected_species.replace(' ', '_')}_map.html"
        map_url = f"http://127.0.0.1:8001/maps/{map_filename}"
        return ui.HTML(f'<h3 style="font-weight: bold;">{selected_species}</h3><iframe src="{map_url}" width="100%" height="600px"></iframe>')

    @output
    @render.ui
    def linegraph_display():
        selected_species = input.species()
        if not selected_species:
            return None

        # Format the plot filename based on the species name
        plot_filename = f"{selected_species.replace(' ', '_')}.png"
        
        # Correct URL for the line graph image, ensuring it's relative to the FastAPI app's static path
        plot_url = f"http://127.0.0.1:8001/species_plots/{plot_filename}"  # Full URL path

        # Return HTML to display the image
        return ui.HTML(f'</h3><img src="{plot_url}" width="100%" />')

app = App(app_ui, server)

if __name__ == "__main__":
    app.run(port=8000)
