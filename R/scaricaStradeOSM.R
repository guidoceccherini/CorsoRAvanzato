library(osmextract)
library(sf)

# 1. Cerca e scarica la regione del Veneto da OpenStreetMap (fonte Geofabrik)
# 'oe_get()' gestisce automaticamente il download e il ritaglio sui confini amministrativi
veneto_roads <- oe_get(
  place = "Veneto", 
  layer = "lines",       # 'lines' contiene le strade
  query = "SELECT * FROM lines WHERE highway IS NOT NULL",
  download_only = FALSE
)

# 2. Visualizza un'anteprima delle prime righe e delle tipologie di strade
print(head(veneto_roads))
table(veneto_roads$highway)
 veneto_roadsh <- veneto_roads |> filter(highway  == 'motorway')

st_write(veneto_roadsh , "Data/strade_veneto.gpkg", append = FALSE)
