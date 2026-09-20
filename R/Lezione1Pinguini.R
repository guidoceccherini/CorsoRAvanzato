# ==========================================================================
# Lezione 1 - Fondamenti del tidyverse e preparazione dei dati
# SLIDE FINALI 1-7: Esercizio di pulizia e sintesi con {palmerpenguins}
# ==========================================================================

library(tidyverse)   # dplyr, tidyr, ggplot2, purrr...
library(lubridate)
# install.packages("palmerpenguins")  # se non già installato
library(palmerpenguins)

# --------------------------------------------------------------------------
# SLIDE 1 - Il dataset e la domanda di ricerca
# --------------------------------------------------------------------------
# 344 pinguini, 3 specie (Adelie, Chinstrap, Gentoo), 3 isole
# dell'arcipelago Palmer (Antartide). Domanda guida: come variano le misure
# corporee tra specie/isole e nel tempo di campionamento?

glimpse(penguins)

# Concetto di tidy data:
# - ogni riga = un pinguino osservato (unità statistica)
# - ogni colonna = una variabile (specie, isola, misure biometriche...)
# - ogni cella = un valore

# --------------------------------------------------------------------------
# SLIDE 2 - Ispezione e gestione dei missing values
# --------------------------------------------------------------------------

# Conteggio NA per colonna
penguins %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  glimpse()

# Pulizia: rimuovo righe con NA solo sulle variabili numeriche chiave
penguins_clean <- penguins %>%
  drop_na(bill_length_mm, bill_depth_mm, flipper_length_mm,
          body_mass_g, sex)

nrow(penguins)        # dataset originale
nrow(penguins_clean)  # dopo la pulizia

# --------------------------------------------------------------------------
# SLIDE 3 - I verbi dplyr in pipeline
# --------------------------------------------------------------------------

penguins_derived <- penguins_clean %>%
  filter(body_mass_g > 3000) %>%                 # filter: sottoinsieme righe
  mutate(bill_ratio = bill_length_mm / bill_depth_mm,  # mutate: nuova var.
         flipper_mass_ratio = flipper_length_mm / body_mass_g * 1000) %>%
  select(species, island, sex, year,              # select: sottoinsieme colonne
         body_mass_g, bill_ratio, flipper_mass_ratio) %>%
  arrange(desc(body_mass_g))                       # arrange: ordinamento

head(penguins_derived, 10)

# --------------------------------------------------------------------------
# SLIDE 4 - group_by() + summarise(): statistiche per specie e isola
# --------------------------------------------------------------------------

summary_species_island <- penguins_clean %>%
  group_by(species, island) %>%
  summarise(
    n_ind        = n(),
    mass_mean    = mean(body_mass_g),
    mass_sd      = sd(body_mass_g),
    flipper_mean = mean(flipper_length_mm),
    .groups = "drop"
  ) %>%
  arrange(species, desc(mass_mean))

summary_species_island

# --------------------------------------------------------------------------
# SLIDE 5 - Join relazionale: integro metadati delle isole
# --------------------------------------------------------------------------

island_info <- tibble(
  island       = c("Biscoe", "Dream", "Torgersen"),
  area_km2     = c(20.0, 1.6, 0.3),
  dist_base_km = c(12, 25, 8)
)

summary_with_island <- summary_species_island %>%
  left_join(island_info, by = "island")

summary_with_island

# Nota didattica: left_join() mantiene tutte le righe della tabella a
# sinistra e aggiunge le colonne corrispondenti dalla tabella a destra,
# esattamente come quando si integrano dati di campo con metadati di sito.

# --------------------------------------------------------------------------
# SLIDE 6 - Rimodellamento dei dati: pivot_longer() e pivot_wider()
# --------------------------------------------------------------------------

# Da wide a long: utile per confrontare più misure nello stesso grafico
penguins_long <- penguins_clean %>%
  select(species, island, sex,
         bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  pivot_longer(
    cols = bill_length_mm:body_mass_g,
    names_to  = "misura",
    values_to = "valore"
  )

head(penguins_long, 8)

# Da long a wide: confronto specie x isola in tabella comparativa
summary_wide <- summary_species_island %>%
  select(species, island, mass_mean) %>%
  pivot_wider(
    names_from  = island,
    values_from = mass_mean
  )

summary_wide

# --------------------------------------------------------------------------
# SLIDE 7 - lubridate + plot finale
# --------------------------------------------------------------------------

# Nel dataset reale non ci sono date esatte di campionamento, solo l'anno.
# Simuliamo una data plausibile per illustrare le funzioni di lubridate.
set.seed(42)
penguins_dated <- penguins_clean %>%
  mutate(
    sampling_date = make_date(year, sample(10:12, n(), replace = TRUE),
                              sample(1:28, n(), replace = TRUE)),
    sampling_month = month(sampling_date, label = TRUE),
    sampling_year  = year(sampling_date)
  )

penguins_dated %>%
  select(species, sampling_date, sampling_month, sampling_year) %>%
  head()

# Plot finale: distribuzione della massa corporea per specie e isola
ggplot(penguins_clean,
       aes(x = species, y = body_mass_g, fill = island)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1) +
  labs(
    title = "Massa corporea dei pinguini per specie e isola",
    subtitle = "Dataset palmerpenguins - dopo pulizia con tidyverse",
    x = "Specie", y = "Massa corporea (g)", fill = "Isola"
  ) +
  theme_minimal(base_size = 13)

# Plot 2: relazione tra due misure biometriche, per specie
ggplot(penguins_clean,
       aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Lunghezza pinna vs massa corporea",
    x = "Lunghezza pinna (mm)", y = "Massa corporea (g)", color = "Specie"
  ) +
  theme_minimal(base_size = 13)
