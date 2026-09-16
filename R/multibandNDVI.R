
# ==============================================================================
# CREAZIONE RASTER MULTIBANDA NDVI
# ==============================================================================

library(terra)


# ==============================================================================
# 1. DIRECTORY DEI RASTER NDVI
# ==============================================================================

# Directory dove hai scaricato i 12 GeoTIFF da Google Earth Engine
ndvi_dir <- "Data"

# Output
output_file <- "multibandNDVI.tiff"


# ==============================================================================
# 2. TROVA I 12 FILE NDVI
# ==============================================================================

ndvi_files <- list.files(
  path = ndvi_dir,
  pattern = "Veneto_NDVI_Climatology_.*\\.tif$",
  full.names = TRUE
)

# Visualizza i file trovati
print(ndvi_files)


# ==============================================================================
# 3. ORDINA I FILE PER MESE
# ==============================================================================

# ATTENZIONE:
# list.files() non garantisce necessariamente l'ordine desiderato.
#
# Cerchiamo quindi il numero del mese contenuto nel nome del file.

month_number <- as.integer(
  sub(
    ".*Month_([0-9]{2}).*",
    "\\1",
    basename(ndvi_files)
  )
)

# Ordina
ndvi_files <- ndvi_files[
  order(month_number)
]

month_number <- month_number[
  order(month_number)
]


# Controllo
print(
  data.frame(
    file = basename(ndvi_files),
    month = month_number
  )
)


# ==============================================================================
# 4. CONTROLLO: DEVONO ESSERCI 12 FILE
# ==============================================================================

if (length(ndvi_files) != 12) {
  
  stop(
    paste0(
      "Errore: trovati ",
      length(ndvi_files),
      " raster NDVI invece di 12."
    )
  )
}


# ==============================================================================
# 5. LEGGI I RASTER
# ==============================================================================

ndvi <- rast(
  ndvi_files
)


# ==============================================================================
# 6. CONTROLLO GEOMETRIA
# ==============================================================================

print(ndvi)

cat("\n")
cat("Numero di bande:", nlyr(ndvi), "\n")
cat("Risoluzione:", res(ndvi), "\n")
cat("CRS:", crs(ndvi), "\n")
cat("Estensione:", as.character(ext(ndvi)), "\n")


# ==============================================================================
# 7. CONTROLLA CHE TUTTE LE BANDE ABBIANO LA STESSA GEOMETRIA
# ==============================================================================

reference <- ndvi[[1]]

for (i in 2:nlyr(ndvi)) {
  
  same_geometry <- compareGeom(
    reference,
    ndvi[[i]],
    stopOnError = FALSE,
    messages = FALSE
  )
  
  if (!same_geometry) {
    
    stop(
      paste(
        "Errore: il raster del mese",
        month_number[i],
        "non ha la stessa geometria del primo raster."
      )
    )
  }
}


# ==============================================================================
# 8. NOMI DELLE BANDE
# ==============================================================================

names(ndvi) <- paste0(
  "NDVI_",
  sprintf("%02d", 1:12)
)

print(names(ndvi))


# ==============================================================================
# 9. SCRIVI IL RASTER MULTIBANDA
# ==============================================================================

writeRaster(
  ndvi,
  output_file,
  overwrite = TRUE,
  datatype = "FLT4S",
  gdal = c(
    "COMPRESS=DEFLATE",
    "PREDICTOR=3"
  )
)


# ==============================================================================
# 10. RILEGGI IL FILE CREATO
# ==============================================================================

multibandNDVI <- rast(
  output_file
)


# ==============================================================================
# 11. CONTROLLO FINALE
# ==============================================================================

print(multibandNDVI)

cat("\n")
cat("============================================\n")
cat("MULTIBAND NDVI CREATO\n")
cat("============================================\n")
cat("File:", output_file, "\n")
cat("Numero bande:", nlyr(multibandNDVI), "\n")
cat("Bande:", paste(names(multibandNDVI), collapse = ", "), "\n")
cat("Risoluzione:", paste(res(multibandNDVI), collapse = " x "), "\n")
cat("============================================\n")


# ==============================================================================
# 12. VISUALIZZAZIONE DI ALCUNE BANDE
# ==============================================================================

plot(
  multibandNDVI[[1]],
  main = "NDVI climatologico - Gennaio"
)

plot(
  multibandNDVI[[7]],
  main = "NDVI climatologico - Luglio"
)
