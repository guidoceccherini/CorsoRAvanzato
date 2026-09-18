library(terra)
library(leaflet)

# 1. Load your UTM raster 
rasterNDVI_utm <- rast('Data/Multiband/rasterNDVI_utm.tiff')

# 2. Project back to EPSG:4326 for web mapping
rasterNDVI_wgs84 <- project(rasterNDVI_utm, "EPSG:4326", method = "bilinear")

# 3. Pick one layer to plot (e.g., Layer 7, which corresponds to July)
ndvi_july <- rasterNDVI_wgs84[[7]]

# 4. Define a color palette for NDVI (from barren/low to dense vegetation)
pal <- colorNumeric(
  palette = c("brown", "yellow", "green", "darkgreen"), 
  domain = values(ndvi_july), 
  na.color = "transparent"
)

# 5. Render in Leaflet
leaflet() %>%
  addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>%
  addRasterImage(
    ndvi_july, 
    colors = pal, 
    opacity = 0.8, 
    project = FALSE # Set to FALSE because we already reprojected using terra!
  ) %>%
  addLegend(
    pal = pal, 
    values = values(ndvi_july), 
    title = "NDVI (July)"
  )
