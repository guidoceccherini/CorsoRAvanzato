
library(sf)
library(dplyr)
library(lubridate)
library(terra)

rasterNDVI <- rast('Data/Multiband/multibandNDVI.tiff')

# 2. Define the target UTM CRS (UTM Zone 33N for Northern Italy is EPSG:32633)
target_crs <- "EPSG:32633"

# 3. Reproject the raster using bilinear interpolation
rasterNDVI_utm <- project(rasterNDVI, target_crs, method = "bilinear")

# 4. Verify the new projection and resolution
print(rasterNDVI_utm)


writeRaster(rasterNDVI_utm,'Data/Multiband/rasterNDVI_utm.tiff')
