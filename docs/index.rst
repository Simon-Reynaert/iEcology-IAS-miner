Welcome to iEcology IAS Miner Documentation
===========================================

**iEcology-IAS-miner** allows users to seamlessly extract data from a variety 
of internet platforms on invasive species mentions and process them to daily 
counts per species × country × platform datasets.

It creates timeseries datasets recording changes in invasive alien species (IAS) 
searches, mentions, posts, pageviews and activity from **Wikipedia**, **Flickr**, 
**Youtube** and **iNaturalist**. Reference data is also pulled from **EASIN** 
and **GBIF**.

Installation
============

.. code-block:: bash

   git clone https://github.com/Simon-Reynaert/iEcology-IAS-miner.git
   cd iEcology-IAS-miner
   python -m venv .venv
   source .venv/bin/activate  # On Windows: .venv\Scripts\activate
   pip install -r requirements.txt

Demonstrations
===========

.. toctree::
   :maxdepth: 1
   
   scripts/workflow_demonstration
   scripts/Demonstration_flickr_img_to_obs
   
Contents
========

.. toctree::
   :maxdepth: 2
   :caption: Modules:

   activity_mining
   data_processing
   easin_mining
   flickr_to_plantnet
   list_mining

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`