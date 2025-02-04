import sys 
import subprocess
import importlib

# List of required libraries
required_libraries = ["shiny", "fastapi", "starlette", "uvicorn", "pandas"]

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
from pathlib import Path
import pandas as pd
from shiny import App, ui, render
from fastapi import FastAPI
from starlette.staticfiles import StaticFiles
import uvicorn
import threading

# Paths
map_dir = Path("maps")

def ensure_directory_exists(directory):
    if not directory.exists():
        directory.mkdir()

ensure_directory_exists(map_dir)

# Load species data to determine top 8 species with most red-colored countries
def get_top_species():
    df = pd.read_csv("species_pageviews_analysis_2025_01.csv")
    red_counts = df[df["Status"] == "Increasing"].groupby("Scientific Name").size()
    top_species = red_counts.nlargest(8)
    return top_species

top_species = get_top_species()

# FastAPI app to serve maps
fastapi_app = FastAPI()
fastapi_app.mount("/maps", StaticFiles(directory=str(map_dir)), name="maps")

def run_fastapi():
    uvicorn.run(fastapi_app, host="127.0.0.1", port=8001)

threading.Thread(target=run_fastapi, daemon=True).start()

# Function to generate color scale (red to yellow)
def get_color(value, max_value):
    ratio = value / max_value
    red = 255
    green = int(255 * (1 - ratio))
    return f"rgb({red}, {green}, 0)"

# UI Layout
app_ui = ui.page_fluid(
    ui.h2("Species Status Interactive Map Viewer"),
    ui.input_select("species", "Select a species:", list(top_species.index)),
    ui.output_ui("map_display"),
    ui.output_text("error_message"),
    ui.h2("Top 8 Species with Most Red-Colored Countries"),
    ui.layout_column_wrap(
        width=4,
        *[
            ui.card(
                ui.h4(species),
                ui.p(f"{count} countries affected"),
                style=f"background-color: {get_color(count, top_species.max())}; padding: 20px; text-align: center; color: black; cursor: pointer;",
                onclick=f"Shiny.setInputValue('species', '{species}')"
            )
            for species, count in top_species.items()
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
        return ui.HTML(f'<h3>{selected_species}</h3><iframe src="{map_url}" width="100%" height="600px"></iframe>')

app = App(app_ui, server)

if __name__ == "__main__":
    app.run(port=8000)

