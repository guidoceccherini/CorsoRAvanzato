library(purrr)
library(readr)
library(dplyr)
library(tidyr)
library(broom)

## --- Step 1: funzione personalizzata -------------------------------------
# Classifica il vigore vegetativo in base al valore di NDVI
classifica_vigore <- function(ndvi) {
  vigore <- ifelse(ndvi > 0.6, "alto", "basso")
  return(vigore)
}

# Test rapido
classifica_vigore(0.75)  # "alto"
classifica_vigore(0.42)  # "basso"


## --- Step 2: importare solo i file corretti con un pattern regex ---------
# Il pattern esclude "note_metadati.csv" e prende solo le stazioni forestali
file_csv <- list.files("foreste", pattern = "^Stazione_forestale_.*\\.csv$",
                       full.names = TRUE)
file_csv

# Sintassi moderna consigliata (map + list_rbind) al posto di map_dfr()
dati_foreste <- map(file_csv, read_csv) |> list_rbind()
dati_foreste


## --- Step 3: sostituire un ciclo for con map_chr() / map_dbl() -----------
# Classificazione del vigore per ogni osservazione
dati_foreste <- dati_foreste %>%
  mutate(vigore = map_chr(ndvi, classifica_vigore))

# Biomassa media per tipo forestale (al posto di un ciclo for con preallocazione)
biomassa_media_per_tipo <- dati_foreste %>%
  group_by(tipo_forestale) %>%
  summarise(biomassa_media = mean(biomassa_t_ha))

biomassa_media_per_tipo


## --- Step 4: nest() + mutate() + map() -> un modello per tipo forestale --
modelli_foresta <- dati_foreste %>%
  group_by(tipo_forestale) %>%
  nest() %>%
  mutate(
    modello = map(data, ~ lm(biomassa_t_ha ~ ndvi, data = .x))
  )

modelli_foresta


## --- Step 5: estrarre i coefficienti con broom::tidy() e riflettere ------
coeff_foreste <- modelli_foresta %>%
  mutate(tidied = map(modello, ~ tidy(.x))) %>%
  select(tipo_forestale, tidied) %>%
  unnest(tidied)

coeff_foreste

# Bonus: qualita' di adattamento (R^2) per tipo forestale, con broom::glance()
qualita_modelli <- modelli_foresta %>%
  mutate(glance_out = map(modello, ~ glance(.x))) %>%
  select(tipo_forestale, glance_out) %>%
  unnest(glance_out) %>%
  select(tipo_forestale, r.squared, p.value)
