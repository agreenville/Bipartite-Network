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

```Safariland.csv  
Safariland_Abund.csv``` (has plant abundance data in the last column)  

### Summary of setup steps:

1.	Download R and install it.  

2.	Download RStudio and install it.  

3.	Install required R packages:

``` install.packages(c("shiny", "bipartite""), dependencies = TRUE) ```

4.	Download/clone network app from github and unzip it somewhere safe:

### To run app via RStudio: 

1.	Open RStudio project 

``` Network.Rproj ```

2.	Open script  

```app.R ```

3.	Click run app 

[Click on arrow](https://github.com/agreenville/Bipartite-Network/blob/master/user_guide/run-app-menu.PNG?raw=TRUE)

Need to make sure it runs “externally” i.e. in the web browser for full functionality.

[Check](user_guide/run-app-menu2.PNG?raw=true) 




