library(geodata)
library(sf)

# 1. Scarica i confini amministrativi dell'Italia direttamente da GADM (livello 1 = Regioni)
# Salva i dati nella cartella di lavoro corrente (path = ".")
italia_gadm <- gadm(country = "ITA", level = 1, path = ".")

# 2. Converti l'oggetto in un formato vettoriale standard 'sf' se preferisci lavorarci con sf
italia_sf <- st_as_sf(italia_gadm)

# 3. Estrai solo il Veneto
veneto_gadm <- italia_sf[italia_sf$NAME_1 == "Veneto", ]

# Visualizza
plot(st_geometry(veneto_gadm), main = "Confine Veneto da GADM")


# Installa i pacchetti necessari (se non già presenti)
# install.packages(c("geodata", "terra", "sf", "dplyr"))

library(geodata)
library(terra)
library(sf)
library(dplyr)

# ---------------------------------------------------------
# 1. CONFINI E CAPOLUOGHI DI PROVINCIA (GADM Livello 2)
# ---------------------------------------------------------
# Scarichiamo il livello 2 dell'Italia (Province) per identificare i capoluoghi del Veneto
prov_ita <- gadm(country = "ITA", level = 2, path = ".")
prov_sf <- st_as_sf(prov_ita)

# Filtriamo per la regione Veneto e troviamo i centroidi o i nomi delle province/capoluoghi
veneto_prov <- prov_sf %>% filter(NAME_1 == "Veneto")

# I capoluoghi di provincia del Veneto con le relative informazioni amministrative:
# Venezia, Verona, Padova, Vicenza, Treviso, Rovigo, Belluno
capoluoghi <- veneto_prov %>%
  select(NAME_1, NAME_2, HASC_2) # NAME_2 contiene solitamente il nome della provincia/capoluogo

print("Capoluoghi / Province del Veneto:")
print(veneto_prov$NAME_2)


# ---------------------------------------------------------
# 2. POPOLAZIONE (WorldPop)
# ---------------------------------------------------------
# Scarichiamo la griglia di popolazione per l'Italia (risoluzione 1 km circa)
pop_ita <- population(year = 2020, resolution = "1", country = "ITA", path = ".")

# Ritagliamo e mascheriamo sul Veneto
veneto_vect <- vect(veneto_prov) # Convertiamo in SpatVector per terra
pop_veneto <- crop(pop_ita, veneto_vect)
pop_veneto <- mask(pop_veneto, veneto_vect)

# Popolazione totale stimata in Veneto (somma dei pixel)
tot_pop_veneto <- global(pop_veneto, "sum", na.rm = TRUE)
cat("Popolazione stimata in Veneto (2020):", round(tot_pop_veneto$sum), "\n")


# ---------------------------------------------------------
# 3. CLIMA STORICO (WorldClim - Medie 1970-2000)
# ---------------------------------------------------------
# Scarichiamo le variabili bioclimatiche (es. temperatura e precipitazioni) alla massima risoluzione (0.5 minuti d'arco)
clima_ita <- worldclim_country(country = "ITA", var = "bio", res = 0.5, path = ".")

# Ritagliamo sul Veneto
clima_veneto <- crop(clima_ita, veneto_vect)
clima_veneto <- mask(clima_veneto, veneto_vect)

# Le 19 variabili bioclimatiche (es. Bio 1 = Temperatura Media Annua, Bio 12 = Precipitazione Totale Annua)
# Estraiamo ad esempio la temperatura media annuale per i capoluoghi
# (Convertiamo prima i poligoni in punti centrali/centroidi)
centri_prov <- st_centroid(veneto_prov)
valori_clima <- extract(clima_veneto, vect(centri_prov))

# Aggiungiamo i dati climatici estratti ai capoluoghi
capoluoghi_clima <- cbind(veneto_prov, valori_clima)
print(capoluoghi_clima[, c("NAME_2", "wc2.1_30s_bio_1")]) # bio_1 è la temperatura media (*10)


df_capoluoghi <- data.frame(
  nome = c("Venezia", "Verona", "Padova", "Vicenza", "Treviso", "Rovigo", "Belluno"),
  lat  = c(45.4371, 45.4384, 45.4064, 45.5456, 45.6669, 45.0705, 46.1425),
  lon  = c(12.3326, 10.9916, 11.8768, 11.5428, 12.2435, 11.7900, 12.2158)
)

# Salva in formato CSV
write.csv(df_capoluoghi, "capoluoghi_veneto_esatte.csv", row.names = FALSE, fileEncoding = "UTF-8")

print("File CSV 'capoluoghi_veneto_esatte.csv' creato con successo!")