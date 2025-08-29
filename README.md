[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Tests Passing](https://github.com/Simon-Reynaert/iEcology-IAS-miner/actions/workflows/tests.yml/badge.svg?branch=main)](https://github.com/Simon-Reynaert/iEcology-IAS-miner/actions/workflows/tests.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

# iEcology-IAS-miner

Copyright (C) 2025-2026 [Simon Reynaert](https://scholar.google.be/citations?user=yIVCDfoAAAAJ&hl=en).

Project website: https://onestop-project.eu/

The goal of **iEcology-IAS-miner** is to allow users to seamlessly extract data from a variety of internet platforms on invasive species mentions and process them to daily counts per species x country x platform datasets. Specifically, it creates timeseries datasets recording changes in invasive species searches, pageviews and activity between 2016 and 2025 from Wikipedia, Flickr, Youtube and iNaturalist. Reference data on the same species x country combinations is also pulled from EASIN and GBIF.  

Currently this repository is still a work in progress. Soon it will be integrated within the OneSTOP repository.  

| __Publications About the Library__ | TBA |
| :--- | :--- |
| __Packages and Releases__ |  [![GitHub release (latest by date)](https://img.shields.io/github/v/release/Simon-Reynaert/iEcology-IAS-miner?logo=GitHub)](https://github.com/Simon-Reynaert/iEcology-IAS-miner/releases) |
| __Pytest Test Coverage__ | [![coverage](https://img.shields.io/codecov/c/github/Simon-Reynaert/iEcology-IAS-miner/main?style=flat-square)](https://codecov.io/gh/Simon-Reynaert/iEcology-IAS-miner) |
| __Security__ | TBA |
| __DOI__ | TBA |
| __Other Information__ | TBA |
| __Support__ | TBA |

# Installation
You can install the development version of iEcology-IAS-miner from Github with:
```bash
# Clone the repository
git clone https://github.com/Simon-Reynaert/iEcology-IAS-miner.git
cd iEcology-IAS-miner

# Optional: create a virtual environment
python -m venv .venv
# Activate the environment (Windows)
.\.venv\Scripts\activate
# Activate the environment (Linux/macOS)
source .venv/bin/activate

# Install dependencies
pip install --upgrade pip
pip install -r requirements.txt
```
# Demonstration
This Jupiter notebook file demonstrates the computation and visualizations of iEcology-IAS-miner, utilizing its different functions. 
