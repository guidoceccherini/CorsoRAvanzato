library(readr)
library(dplyr)

# Crea la cartella di lavoro se non esiste
dir.create("Data/esercizio", showWarnings = FALSE)

# Funzione helper per generare dati simulati
genera_dati <- function(n_file) {
  set.seed(2026 + n_file)  # riproducibilità±¹
  
  data_seq <- seq(
    from = as.Date("2025-01-01"),
    by = "day",
    length.out = 30
  )
  
  stazioni <- c("ST001", "ST002", "ST003", "ST004")
  
  dati <- tibble(
    data = sample(data_seq, 50, replace = TRUE),
    stazione = sample(stazioni, 50, replace = TRUE),
    valore = round(rnorm(50, mean = 100, sd = 15), 2)
  ) %>%
    arrange(data, stazione)
  
  return(dati)
}

# Genera e salva 3 file CSV
for (i in 1:3) {
  nome_file <- sprintf("dati_%02d.csv", i)
  percorso <- file.path("Data/esercizio", nome_file)
  
  dati_i <- genera_dati(i)
  
  write_csv(dati_i, percorso)
  
  message("Creato: ", percorso, " (", nrow(dati_i), " righe)")
}

message("\nFile pronti per l'esercizio map_dfr().")
message("Esempio di utilizzo in aula:")
message('  file_csv <- list.files("esercizio", pattern = "\\\\.csv$", full.names = TRUE)')
# message('  dati_unici <- map_dfr(file_csv, read_csv)'
        