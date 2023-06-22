
September 12th 2022
Kendalynn A. Morris
This folder contains all the scripts and data from field measurements associated with the PairedMethane project.


Contents:
Folders
original licor data
	raw .json files and zipped file with corrected collar heights and recalculated fluxes,
	exported contents of the zip file are in export.csv
miscellaneous
	spreadsheets from collar height measuremetns, R script for finding the average height for each collar
	script to determine the identity of collars 639 and 693 for which the original flag was illegible
CSVs
July22_soilmoisture.csv
	soil samples to calibrate SWC from probe to true gravimetric water contetn
transplant_soil_chemistry_042020.csv
	data from Hopple et al 2022 soil chemical analysis
cores_collars.csv
	spreadsheet listing collar numbers and their treatment ids
export.csv
	exported from Soil Flux Pro
licordata.csv
	same as export.csv but with minor reformatting
licorRTA.csv
	processed through PairedMethaneLiCorData script, ready-to-analyze
Scripts
PairedMethaneLiCorData
	removes bad values (e.g., fluxes of -9999) and summarizes by collar and sampling day
LicorDataAnalysis
	tests for effect of location and disturbance history, as well as soil water content and temperature on fluxes
Figures
	graphs of data, emphasis on tested effects
SoilCollectionEffect
	testing the similarity of fluxes measured pre and post soil collection on July 27th and 29th,
	due to increased variance post collection, only the first flux measurements were kept in RTA dataset
