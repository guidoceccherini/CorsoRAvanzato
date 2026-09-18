library(sf)
library(dplyr)
library(lubridate)
library(terra)


Corine <- rast('Data/Corine_LandCover_Veneto_2018.tif')
Dem <- rast('Data/Veneto_SRTM_DEM_90m.tif')

Dem_utm <- project(Dem, crs(Corine))

ext(Dem_utm)
ext(Corine)
