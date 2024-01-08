The scripts in this repo consist of the code needed to download and clean SSURGO data used for the pasture yield prediction model associated with the Grasslands 2.0 project.

The first script needed is make_WI_grass_soil.R. This script downloads SSURGO data with the package FedData, and then cleans the data by selecting variables of interest and creates new variables by summarizing properties for the first 30 cm of soil depth.



