# Bipartite Network  
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1205201.svg)](https://doi.org/10.5281/zenodo.1205201)

The Bipartite Network analysis app was created with Shiny and Bipartite package for R. The app was made to visualise a bipartite network  and generate nestedness indices for student projects.

### Features:
- Supports data matrices with and without abundance data of lower-order group/species.
- Generates bipartite graph from interaction matrix.
- Calculates nestedness (NODF2) indices.
- Visualises interaction matrix. Shading indicates number of interactions.
- Download graphs and matrices and save to your device.

### Data file format:

Save data file as a .csv, with rows as higher order group (e.g. plant species) and columns as lower order group (e.g. invertebrate species). If you have the abundance of each higher order group (e.g. plant species), add abundance data as the last column.

You can see the app in action here: https://aaronecology.shinyapps.io/Network/  
  
Example datasets from the Bipartite package:

Safariland.csv  
Safariland_Abund.csv (has plant abundance data in the last column)  
