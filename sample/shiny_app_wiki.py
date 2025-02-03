import os
from pathlib import Path
from shiny import App, ui, render
from fastapi import FastAPI
from starlette.staticfiles import StaticFiles
import uvicorn
import threading

# Paths
map_dir = Path("maps")

# Ensure the output directory exists
if not map_dir.exists():
    map_dir.mkdir()

# Get available species from pre-generated maps
species_list = [f.stem.replace("_map", "").replace("_", " ") for f in map_dir.glob("*.html")]

# FastAPI app to serve maps
fastapi_app = FastAPI()
fastapi_app.mount("/maps", StaticFiles(directory=str(map_dir)), name="maps")

# Start FastAPI in a separate thread
def run_fastapi():
    uvicorn.run(fastapi_app, host="127.0.0.1", port=8001)

threading.Thread(target=run_fastapi, daemon=True).start()

# UI Layout
app_ui = ui.page_fluid(
    ui.h2("Species Status Interactive Map Viewer"),
    ui.input_select("species", "Select a species:", species_list),
    ui.output_ui("map_display"),
    ui.output_text("error_message")
)

# Server Logic
def server(input, output, session):
    @output
    @render.ui
    def map_display():
        selected_species = input.species()
        if not selected_species:
            return ui.HTML("Select a species to display its map.")

        # Construct the correct URL for the map file
        map_filename = f"{selected_species.replace(' ', '_')}_map.html"
        map_url = f"http://127.0.0.1:8001/maps/{map_filename}"

        return ui.HTML(f'<iframe src="{map_url}" width="100%" height="600px"></iframe>')

# Run the Shiny App
app = App(app_ui, server)

if __name__ == "__main__":
    app.run(port=8000)
