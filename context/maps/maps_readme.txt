# Maps readme 	

This folder contains maps that can be used for the production of graphics on election day. The files are as follows: 

## counties_centroids.rds 

This is a file that contains the point data for county centroids within the U.S., along with population weights. When combined with the maps package in R's county data, it can be used to create bubble/dot cartogram maps. 

## hexmap/us_states_hexgrid.geojson 
These are files of the United States in a hexagonal pattern. These can be used to present quick results by state as to the election results while keeping all of the states as the same size, allowing for greater clarity for the viewer. 

## dma_continental.rds/dem_esri folder + files 

These data are maps of the United States metropolitan areas and designated market areas (DMAs). These can be used to talk about results by general metro area, while additionally having the names match up to the output of google analytics trends data when selecting the metro layer. These contain the following fields of interest: 

### NAME - The name of the metro area

### DMA - The google number of the metro area

### USH_AREA - Area of the DMA in square meters 

### dma_metro - The google name for the metro/DMA area; for merging on google analytics data 

### trump_dma - The areal weighted calculated total of Trump votes estimated for the DMA from 2016. 

### totalvotes_dma - The areal weighted calculated total number of votes estimated for the DMA from 2016.

### hispanic_dma_pct - The areal weighted calculated estimated hispanic percentage of the DMA 

### white_dma_pct - The areal weighted calculated estimated white percentage of the DMA 

### pop2010dma - The Census population for the DMA 

### trump_pct - The areal weighted calculated percentage of Trump votes from 2016

### area - area in square kilometers 

### log_pop_sqkm - The logged population per square kilometer for the DMA 

