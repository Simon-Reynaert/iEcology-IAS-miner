[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Tests Passing](https://github.com/Simon-Reynaert/iEcology-IAS-miner/actions/workflows/tests.yml/badge.svg?branch=main)](https://github.com/Simon-Reynaert/iEcology-IAS-miner/actions/workflows/tests.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Docs](https://readthedocs.org/projects/iecology-ias-miner/badge/?version=latest)](https://iecology-ias-miner.readthedocs.io/en/latest/index.html)

# iEcology-IAS-miner

Copyright (C) 2025-2026 [Simon Reynaert](https://scholar.google.be/citations?user=yIVCDfoAAAAJ&hl=en).

Project website: https://onestop-project.eu/

The goal of **iEcology-IAS-miner** is to allow users to seamlessly extract data from a variety of internet platforms on invasive species mentions and process them to daily counts per species x country x platform datasets. Specifically, it creates timeseries datasets recording changes in invasive alien species (IAS) searches, mentions, posts, pageviews and activity between a given time range (worked example is 2016 - 2025) from **Wikipedia**, **Flickr**, **Youtube** and **iNaturalist**. Reference data on the same species x country combinations is also pulled from **EASIN** and **GBIF**.  

Currently this repository is still a work in progress. Soon it will be integrated within the OneSTOP repository.  

| __Publications About the Library__ | TBA |
| :--- | :--- |
| __Packages and Releases__ |  [![GitHub release (latest by date)](https://img.shields.io/github/v/release/Simon-Reynaert/iEcology-IAS-miner?logo=GitHub)](https://github.com/Simon-Reynaert/iEcology-IAS-miner/releases) |
| __Pytest Test Coverage__ | [![coverage](https://img.shields.io/codecov/c/github/Simon-Reynaert/iEcology-IAS-miner/main?style=flat-square)](https://codecov.io/gh/Simon-Reynaert/iEcology-IAS-miner) |
| __DOI__ | TBA |
| __Contact Information__ | simon.reynaert@plantentuinmeise.be |
| __Documentation__ | [![Docs](https://readthedocs.org/projects/iecology-ias-miner/badge/?version=latest)](https://iecology-ias-miner.readthedocs.io/en/latest/index.html)|

# Installation
You can install the development version of iEcology-IAS-miner from Github with:
```bash
# Clone the repository
git clone https://github.com/Simon-Reynaert/iEcology-IAS-miner.git
cd iEcology-IAS-miner

# Optional: create a virtual environment (highly recommended!)
python -m venv .venv
# Activate the environment (Windows)
.\.venv\Scripts\activate
# Activate the environment (Linux/macOS)
source .venv/bin/activate

# Install dependencies
pip install --upgrade pip
pip install -r requirements.txt

# Run all tests to verify installation
pytest --maxfail=1 --disable-warnings -q
```
# Demonstration
Code demonstrations can be found on the documentation website (https://iecology-ias-miner.readthedocs.io/en/latest/index.html) or in the /scripts or /docs subdirectories.
