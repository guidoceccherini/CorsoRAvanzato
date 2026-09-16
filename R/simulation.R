# ==============================================================================
# SIMULAZIONE STAZIONI ARPAV + SERIE GIORNALIERE PM10
# ==============================================================================

library(sf)
library(dplyr)
library(lubridate)


# ==============================================================================
# 1. PARAMETRI
# ==============================================================================

# SHP del Veneto precedentemente esportato da GEE
shp_file <- "Data/Veneto_Provinces_GAUL2.shp"

# Directory dove salvare i CSV
output_dir <- "Data/arpav_pm10"

# Numero di stazioni
n_stations <- 100

# Periodo della serie storica
start_date <- as.Date("2001-01-01")
end_date   <- as.Date("2020-12-31")

# Seed per rendere la simulazione riproducibile
set.seed(12345)


# ==============================================================================
# 2. CREA DIRECTORY OUTPUT
# ==============================================================================

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# ==============================================================================
# 3. LEGGI LO SHAPEFILE DEL VENETO
# ==============================================================================

veneto <- st_read(
  shp_file,
  quiet = FALSE
)

print(veneto)

# Controlla CRS
print(st_crs(veneto))


# ==============================================================================
# 4. CREA UN'UNICA GEOMETRIA DEL VENETO
# ==============================================================================

veneto_geom <- veneto |>
  st_make_valid() |>
  st_union()


# ==============================================================================
# 5. CREA 100 PUNTI CASUALI ALL'INTERNO DEL VENETO
# ==============================================================================

stations <- st_sample(
  veneto_geom,
  size = n_stations,
  type = "random"
)

# Converti in sf
stations <- st_as_sf(
  stations
)

# Aggiungi ID stazione
stations <- stations |>
  mutate(
    station_id = sprintf(
      "stazione%03d",
      row_number()
    )
  )


# Visualizza
plot(
  st_geometry(veneto),
  main = "Stazioni ARPAV fittizie"
)

plot(
  st_geometry(stations),
  add = TRUE,
  pch = 20
)


# ==============================================================================
# 6. CREA LA SEQUENZA TEMPORALE
# ==============================================================================

dates <- seq(
  from = start_date,
  to = end_date,
  by = "day"
)

n_days <- length(dates)

print(
  paste(
    "Numero di giorni:",
    n_days
  )
)


# ==============================================================================
# 7. FUNZIONE PER GENERARE UNA SERIE PM10
# ==============================================================================

simulate_pm10 <- function(
    dates,
    station_id
) {
  
  n <- length(dates)
  
  # --------------------------------------------------------------------------
  # COMPONENTE STAGIONALE
  # --------------------------------------------------------------------------
  #
  # PM10 generalmente più elevato in inverno e più basso in estate.
  #
  # cos(2*pi*(DOY)/365) raggiunge il massimo circa in inverno.
  #
  
  doy <- yday(dates)
  
  seasonal <- 20 +
    15 * cos(
      2 * pi * (doy - 15) / 365.25
    )
  
  
  # --------------------------------------------------------------------------
  # TREND TEMPORALE
  # --------------------------------------------------------------------------
  #
  # Leggero declino nel tempo.
  #
  
  year_fraction <-
    as.numeric(
      dates - min(dates)
    ) / 365.25
  
  trend <- -0.3 * year_fraction
  
  
  # --------------------------------------------------------------------------
  # VARIABILITÀ GIORNALIERA
  # --------------------------------------------------------------------------
  
  noise <- rnorm(
    n,
    mean = 0,
    sd = 8
  )
  
  
  # --------------------------------------------------------------------------
  # EPISODI DI INQUINAMENTO
  # --------------------------------------------------------------------------
  #
  # Generiamo alcuni episodi con PM10 molto elevato.
  #
  
  episode_probability <- 0.025
  
  episodes <- rbinom(
    n,
    size = 1,
    prob = episode_probability
  )
  
  episode_intensity <- ifelse(
    episodes == 1,
    rgamma(
      n,
      shape = 2,
      scale = 25
    ),
    0
  )
  
  
  # --------------------------------------------------------------------------
  # EFFETTO STAZIONE
  # --------------------------------------------------------------------------
  #
  # Ogni stazione ha un livello medio leggermente diverso.
  #
  
  station_effect <- rnorm(
    1,
    mean = 0,
    sd = 8
  )
  
  
  # --------------------------------------------------------------------------
  # PM10 FINALE
  # --------------------------------------------------------------------------
  
  pm10 <- seasonal +
    trend +
    noise +
    episode_intensity +
    station_effect
  
  
  # Evita valori negativi
  pm10 <- pmax(
    pm10,
    2
  )
  
  
  # --------------------------------------------------------------------------
  # MISSING DATA
  # --------------------------------------------------------------------------
  #
  # Simuliamo circa il 2% di dati mancanti.
  #
  
  missing <- runif(n) < 0.02
  
  pm10[missing] <- NA
  
  
  # --------------------------------------------------------------------------
  # OUTPUT
  # --------------------------------------------------------------------------
  
  tibble(
    data = dates,
    pm10 = round(
      pm10,
      1
    )
  )
}


# ==============================================================================
# 8. GENERA I 100 CSV
# ==============================================================================

for (i in seq_len(n_stations)) {
  
  station_id <- stations$station_id[i]
  
  # Genera serie
  pm10_data <- simulate_pm10(
    dates = dates,
    station_id = station_id
  )
  
  
  # Nome file
  filename <- file.path(
    output_dir,
    paste0(
      "veneta_arpav_",
      station_id,
      ".csv"
    )
  )
  
  
  # Salva CSV
  write.csv(
    pm10_data,
    filename,
    row.names = FALSE
  )
  
  message(
    "Creato: ",
    filename
  )
}


# ==============================================================================
# 9. SALVA ANCHE LE COORDINATE DELLE STAZIONI
# ==============================================================================

# Converti coordinate in WGS84
stations_wgs84 <- st_transform(
  stations,
  4326
)

coords <- st_coordinates(
  stations_wgs84
)

stations_metadata <- stations_wgs84 |>
  st_drop_geometry() |>
  mutate(
    longitude = coords[, 1],
    latitude = coords[, 2]
  ) |>
  select(
    station_id,
    longitude,
    latitude
  )


write.csv(
  stations_metadata,
  file.path(
    output_dir,
    "veneta_arpav_stations.csv"
  ),
  row.names = FALSE
)


# ==============================================================================
# 10. ESEMPIO DI LETTURA DI UNA STAZIONE
# ==============================================================================

example_file <- file.path(
  output_dir,
  "veneta_arpav_stazione001.csv"
)

example_data <- read.csv(
  example_file
)

head(example_data)

summary(example_data$pm10)


# ==============================================================================
# 11. CONTROLLO RAPIDO
# ==============================================================================

cat("\n")
cat("========================================\n")
cat("SIMULAZIONE COMPLETATA\n")
cat("========================================\n")
cat("Numero stazioni:", n_stations, "\n")
cat("Periodo:", start_date, "->", end_date, "\n")
cat("Numero giorni:", n_days, "\n")
cat("Directory:", output_dir, "\n")
cat("========================================\n")

