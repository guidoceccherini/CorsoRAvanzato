##############################################################################
# LEZIONE 4 — Workflow geospaziale integrato e verifica finale
#
# Tutti i dati usati sono già inclusi nei pacchetti R (nessun download):
#   - sf::nc            -> contee North Carolina (pulizia/diagnosi)
#   - terra "ex/lux.shp"  -> distretti/comuni del Lussemburgo (vettore)
#   - terra "ex/elev.tif" -> elevazione del Lussemburgo (raster allineato)
##############################################################################

# install.packages(c("sf", "terra", "exactextractr", "dplyr", "tidyr",
#                     "ggplot2", "tidyterra", "gt"))

# install.packages("tidyterra", type = "binary")

library(sf)
library(terra)
library(exactextractr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyterra)
library(gt)


##############################################################################
# MODULO 1 — PULIZIA E DIAGNOSI DI DATI REALI
# Dataset: contee North Carolina (sf), noto per problemi di CRS e geometria
##############################################################################

nc_path <- system.file("shape/nc.shp", package = "sf")
nc <- st_read(nc_path, quiet = TRUE)

## 1.1 Diagnosi iniziale ------------------------------------------------------
dim(nc)
names(nc)
st_crs(nc)                       # CRS presente ma da verificare/documentare
sum(is.na(nc$SID74))             # valori mancanti nella variabile di interesse
sum(duplicated(st_geometry(nc))) # geometrie duplicate

## 1.2 Validità geometrica ----------------------------------------------------
validi <- st_is_valid(nc)
table(validi)                    # il dataset nc contiene storicamente geometrie non valide
nc <- st_make_valid(nc)          # ripara le geometrie senza alterare gli attributi
table(st_is_valid(nc))           # verifica post-riparazione

## 1.3 Proiezione per calcoli metrici -----------------------------------------
# nc è in coordinate geografiche: per calcolare aree in km2 serve un CRS proiettato
nc_proj <- st_transform(nc, crs = 32119)  # NAD83 / North Carolina (State Plane, metri)

## 1.4 Coerenza tra area riportata e area ricalcolata -------------------------
nc_proj <- nc_proj %>%
  mutate(area_calc_km2 = as.numeric(st_area(geometry)) / 1e6)

confronto_aree <- nc_proj %>%
  st_drop_geometry() %>%
  select(NAME, AREA, area_calc_km2) %>%
  arrange(desc(area_calc_km2))

head(confronto_aree, 10)

## 1.5 Outlier nel tasso di mortalità SIDS ------------------------------------
nc_proj <- nc_proj %>%
  mutate(tasso_sids74 = SID74 / BIR74 * 1000)

ggplot(nc_proj) +
  geom_boxplot(aes(y = tasso_sids74), fill = "steelblue", alpha = 0.6) +
  labs(title = "Tasso di mortalità SIDS 1974 per contea (NC)",
       y = "Casi per 1000 nati vivi", x = NULL) +
  theme_minimal()

# Contee sopra la soglia di outlier (regola IQR)
q <- quantile(nc_proj$tasso_sids74, c(0.25, 0.75), na.rm = TRUE)
iqr <- diff(q)
soglia_outlier <- q[2] + 1.5 * iqr

nc_proj %>%
  st_drop_geometry() %>%
  filter(tasso_sids74 > soglia_outlier) %>%
  select(NAME, tasso_sids74) %>%
  arrange(desc(tasso_sids74))


##############################################################################
# MODULO 2 — INTEGRAZIONE TABELLARE + VETTORIALE + RASTER
# Dataset: distretti/comuni del Lussemburgo (terra) + elevazione (terra)
##############################################################################

lux_path  <- system.file("ex/lux.shp", package = "terra")
elev_path <- system.file("ex/elev.tif", package = "terra")

lux  <- vect(lux_path)     # SpatVector: poligoni amministrativi
elev <- rast(elev_path)    # SpatRaster: elevazione

## 2.1 Ispezione della struttura ----------------------------------------------
names(lux)                 # colonne disponibili (tipicamente ID_1, NAME_1, ID_2, NAME_2, AREA, ...)
nrow(lux)
crs(lux, describe = TRUE)$name
crs(elev, describe = TRUE)$name
ext(lux)
ext(elev)

## 2.2 Integrazione TABELLARE + VETTORIALE ------------------------------------
# Simuliamo una tabella esterna (es. proveniente da un CSV con dati forestali)
# che nella pratica reale importeresti con read_csv(). Le chiavi (NAME_2) devono
# combaciare ESATTAMENTE: è la parte più delicata di ogni integrazione tabellare.
set.seed(1)
lux_sf <- st_as_sf(lux)

copertura_forestale <- tibble(
  NAME_2 = lux_sf$NAME_2,
  perc_bosco = round(runif(nrow(lux_sf), 15, 55), 1)
)

# Controllo preventivo: ci sono chiavi che non combaciano?
setdiff(lux_sf$NAME_2, copertura_forestale$NAME_2)   # deve essere character(0)

lux_sf <- lux_sf %>%
  left_join(copertura_forestale, by = "NAME_2")

## 2.3 Integrazione VETTORE + RASTER -------------------------------------------
same.crs(lux, elev)              # TRUE: lux.shp ed elev.tif condividono lo stesso CRS

# Visualizzazione integrata: raster di base + confini comunali sopra
ggplot() +
  geom_spatraster(data = elev) +
  geom_sf(data = st_geometry(lux_sf), fill = NA, color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(name = "Elevazione (m)", na.value = "transparent") +
  labs(title = "Elevazione e confini comunali — Lussemburgo") +
  theme_minimal()

## 2.4 Cosa succede se le fonti NON combaciano (dimostrazione didattica) ------
# Esempio "rotto apposta": riproiettiamo il vettore in un CRS diverso e mostriamo
# perché un'operazione congiunta fallirebbe senza un controllo preventivo.
lux_wgs84 <- project(lux, "EPSG:4326")
same.crs(lux_wgs84, elev)        # FALSE: servirebbe project()/reproiezione prima di procedere
# Correzione:
lux_corretto <- project(lux_wgs84, elev)
same.crs(lux_corretto, elev)     # TRUE dopo la correzione


##############################################################################
# MODULO 3 — STATISTICHE ZONALI CON exactextractr
# Confronto tra exact_extract() e terra::zonal()/extract()
##############################################################################

## 3.1 Statistiche zonali di base con exactextractr ---------------------------
stat_zonali <- exact_extract(elev, lux_sf, c("mean", "min", "max", "stdev"),
                             progress = FALSE)

lux_sf <- lux_sf %>%
  bind_cols(stat_zonali) %>%
  rename(elev_media = mean, elev_min = min, elev_max = max, elev_sd = stdev)

lux_sf %>%
  st_drop_geometry() %>%
  select(NAME_1, NAME_2, elev_media, elev_min, elev_max, elev_sd) %>%
  arrange(desc(elev_media)) %>%
  head(10)

## 3.2 Confronto con terra::zonal() (nessuna gestione dei pixel parziali) ----
lux_rast_id <- rasterize(lux, elev, field = "ID_2")   # ogni comune diventa una "zona" raster
confronto_zonal <- zonal(elev, lux_rast_id, fun = "mean", na.rm = TRUE)

confronto <- lux_sf %>%
  st_drop_geometry() %>%
  select(ID_2, NAME_2, elev_media_exact = elev_media) %>%
  left_join(confronto_zonal, by = c("ID_2" = "ID_2")) %>%
  rename(elev_media_zonal = elevation) %>%
  mutate(diff_assoluta = round(elev_media_exact - elev_media_zonal, 2)) %>%
  arrange(desc(abs(diff_assoluta)))

head(confronto, 10)
# I comuni più piccoli/irregolari mostrano tipicamente la differenza maggiore:
# terra::zonal() assegna ogni cella a UN SOLO poligono (quello del centro cella),
# mentre exact_extract() pesa ogni cella per la frazione di area realmente coperta.

## 3.3 Statistica personalizzata: % di superficie comunale sopra una soglia --
# Esempio rilevante per contesti forestali/montani: quota minima di 400 m
perc_sopra_400m <- exact_extract(elev, lux_sf, function(value, cov_frac) {
  sum(cov_frac[value > 400], na.rm = TRUE) / sum(cov_frac, na.rm = TRUE) * 100
})

lux_sf$perc_area_sopra_400m <- round(perc_sopra_400m, 1)

lux_sf %>%
  st_drop_geometry() %>%
  select(NAME_2, perc_area_sopra_400m) %>%
  arrange(desc(perc_area_sopra_400m)) %>%
  head(10)


##############################################################################
# MODULO 4 — PRODUZIONE DI MAPPE E TABELLE FINALI
##############################################################################

## 4.1 Mappa finale pubblicabile ----------------------------------------------
mappa_finale <- ggplot() +
  geom_spatraster(data = elev, alpha = 0.85) +
  geom_sf(data = lux_sf, aes(color = elev_media), fill = NA, linewidth = 2.6) +
  scale_fill_viridis_c(name = "Elevazione (m)", na.value = "transparent") +
  scale_color_viridis_b(name = "Elev. media\nper comune (m)") +
  labs(
    title = "Elevazione e statistiche zonali comunali — Lussemburgo",
    subtitle = "Statistiche calcolate con exactextractr::exact_extract()",
    caption = "Fonte dati: pacchetto terra (esempi inclusi, nessun download esterno)"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

mappa_finale
ggsave("mappa_finale_lussemburgo.png", mappa_finale, width = 9, height = 7, dpi = 300)

## 4.2 Tabella riassuntiva finale ----------------------------------------------
tabella_finale <- lux_sf %>%
  st_drop_geometry() %>%
  select(NAME_1, NAME_2, elev_media, elev_sd, perc_bosco, perc_area_sopra_400m) %>%
  arrange(NAME_1, NAME_2)

tabella_gt <- tabella_finale %>%
  gt(groupname_col = "NAME_1") %>%
  tab_header(
    title = "Statistiche zonali per comune — Lussemburgo",
    subtitle = "Elevazione (exactextractr), copertura forestale simulata"
  ) %>%
  fmt_number(columns = c(elev_media, elev_sd, perc_bosco, perc_area_sopra_400m),
             decimals = 1) %>%
  cols_label(
    NAME_2 = "Comune",
    elev_media = "Elev. media (m)",
    elev_sd = "Elev. dev. std (m)",
    perc_bosco = "% bosco (dato d'esempio)",
    perc_area_sopra_400m = "% area > 400 m"
  )

tabella_gt
gtsave(tabella_gt, "tabella_finale_lussemburgo.html")


##############################################################################
# VERIFICA FINALE (da assegnare agli studenti)
# Stesso dataset, aggregazione diversa: distretti (NAME_1) invece di comuni
##############################################################################

# 1. Ricaricare lux.shp ed elev.tif
# 2. Diagnosticare il dataset vettoriale (CRS, geometrie, valori mancanti)
# 3. Calcolare con exact_extract() media e deviazione standard dell'elevazione
#    per ciascun DISTRETTO (NAME_1), aggregando i comuni al livello superiore
# 4. Produrre una mappa e una tabella finale con i risultati per distretto

# Traccia di soluzione (da non distribuire prima della verifica):
lux_distretti <- lux_sf %>%
  group_by(NAME_1) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_as_sf()

stat_distretti <- exact_extract(elev, lux_distretti, c("mean", "stdev"),
                                progress = FALSE)

lux_distretti <- lux_distretti %>%
  bind_cols(stat_distretti) %>%
  rename(elev_media = mean, elev_sd = stdev)

lux_distretti %>% st_drop_geometry()