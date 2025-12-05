# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# Add your project to the path if you want to document your code
import os
import sys
sys.path.insert(0, os.path.abspath('..'))

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'iEcology-IAS-miner'
copyright = '2025, Simon Reynaert'
author = 'Simon Reynaert'
release = '0.1.0-alpha'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = [
    'sphinx.ext.autodoc',      # For automatic documentation from docstrings
    'sphinx.ext.napoleon',     # For Google/NumPy style docstrings
    'sphinx.ext.viewcode',     # Add links to source code
]

templates_path = ['_templates']
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']



# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

# Use the Read the Docs theme
html_theme = 'alabaster'
html_static_path = ['_static']

