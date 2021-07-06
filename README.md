# Migration-and-Climate-Change

## Summary 
This project aims to better understand how impacts of climate change influence migration patterns. An agent-based modelling framework is developed to capture complex
migration decision-making, social influences on migration decisions, and to explore localized migration scenarios. The model is complemented by an empirical analysis using panel data from rural Ethiopia. 

The model represents a population of household agents residing in different regions. The household agents adapt to changes in their environment by deciding whether to stay or migrate (and if so, where to). Interactions between agents represent social influence on migration decisions. The model is implemented using the spatially explicit GAMA environment (http://gama-platform.org). It should be noted that the model implementation presents a proof-of-concept rather than a comprehensive simulation tool.

## Files
* migration_model.gaml: Contains the model implementation in GAMA.
* ETH_regions.shp: Contains the shapefile with the regions in Ethiopia and regional level data input. 
* regression_analysis.R: Contains the empirical analysis using regression modelling techniques. 
* data.csv: Contains the dataset used for the empirical analysis. The dataset has been constructed by combining survey data (http://www.migratingoutofpoverty.org/themes/migration-data/ethiopiaquant) with climate data (https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview). 
