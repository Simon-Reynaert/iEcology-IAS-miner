import os
import sys

# Add src folder to path
sys.path.insert(0, os.path.abspath('../src'))
sys.path.insert(0, os.path.abspath('../../scripts'))

# Project information
project = 'iEcology IAS Miner'
copyright = '2025, Simon Reynaert'
author = 'Simon Reynaert'
release = '0.1.0'

# Extensions
extensions = [
    'sphinx.ext.autodoc',
    'sphinx.ext.napoleon',
    'sphinx.ext.viewcode',
    'sphinx.ext.autosummary',
    'myst_nb', # to support Jupyter Notebooks
]

# Autosummary settings
autosummary_generate = True

# Use the Read the Docs theme
html_theme = 'sphinx_rtd_theme'
html_static_path = ['_static']

# Other settings
templates_path = ['_templates']
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

#notebook output included
nb_execution_mode="off"

source_suffix = {
    '.rst': 'restructuredtext',
    '.md': 'markdown',
    '.ipynb': 'jupyter_notebook', 
}