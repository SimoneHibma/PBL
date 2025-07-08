#functies in R
packages <- c("sf", "httr", "jsonlite", "lwgeom")
install.packages(setdiff(packages, installed.packages()[, "Package"]))

library(httr)
library(jsonlite)
library(sf)
library(lwgeom)

# ==== 1. Inlezen identificaties ====
document_id <- "/akn/nl/act/pv31/2023/omgevingsvisie" 

# ==== 2. Instellingen ====
endpoint <- "https://service.omgevingswet.overheid.nl/publiek/omgevingsdocumenten/api/presenteren/v7/locaties/_zoek"
api_key <- "" #<---- HIER opvragen, in 1/2 dagen  https://aandeslagmetdeomgevingswet.nl/ontwikkelaarsportaal/formulieren/api-key-aanvragen-0/
base_dir <- "C:/Users/User/Documents/PBL/output/"  #zelf link naar bestaande map maken
output_path <- paste0(base_dir, document_id_output, ".json")

# ==== 3. JSONs met geoindentificaties opvragen ====
body <- list(
  zoekParameters = list(
    list(
      parameter = "document.identificatie",
      zoekWaarden = list(document_id)
    )
  ),
  page = 0,
  size = 200,
  sort = "noemer,desc",
  toonAlleenWerkingsgebieden = FALSE,
  nietOpgenomenIn = list("omgevingsnorm", "omgevingswaarde", "activiteit", "gebiedsaanwijzing"),
  `_expand` = TRUE,
  `_expandScope` = "omvat.geometrieIdentificaties"
)

# POST-verzoek uitvoeren
response <- POST(
  url = endpoint,
  add_headers(`X-Api-Key` = api_key, `Content-Type` = "application/json"),
  body = toJSON(body, auto_unbox = TRUE)
)

# Verwerken
if (status_code(response) == 200) {
  tekst <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(tekst, simplifyVector = TRUE)
  write_json(data, output_path, pretty = TRUE, auto_unbox = TRUE)
  print(paste("Resultaat opgeslagen in:", output_path))
} else {
  print(paste("Fout bij ophalen locaties. Statuscode:", status_code(response)))
  print(content(response, as = "text", encoding = "UTF-8"))  # extra foutdetails
}

# ==== 4. Geoindentificaties opvragen en samenvoegen in 1 geopackage ====

# input voor ophalen geometrie indentificaties 
document_id_output <- gsub("/", "_", document_id)
input_file <- output_path

# Extract geometrie identificaties
json_data <- fromJSON(input_file, simplifyVector = TRUE)
locaties <- json_data[["_embedded"]][["locaties"]]
geometrie_ids <- locaties[["geometrieIdentificatie"]]

# API details
geo_url <- "https://service.omgevingswet.overheid.nl/publiek/omgevingsdocumenten/api/geometrieopvragen/v1/geometrieen/"
crs_param <- "?crs=http%3A%2F%2Fwww.opengis.net%2Fdef%2Fcrs%2FEPSG%2F0%2F28992"

# Download geometry voor elke geometrie indetificatie 
for (i in seq_along(geometrie_ids)) {
  geo_id <- geometrie_ids[i]
  endpoint <- paste0(geo_url, geo_id, crs_param)
  output_file <- paste0(base_dir, "/", document_id_output, "_", i, ".json")
  
  response <- GET(endpoint, add_headers(`X-Api-Key` = api_key))
  
  if (status_code(response) == 200) {
    tekst <- content(response, as = "text", encoding = "UTF-8")
    data <- fromJSON(tekst, simplifyVector = TRUE)
    write_json(data, output_file, pretty = TRUE, auto_unbox = TRUE)
    cat("Opgeslagen als:", output_file, "\n")
  } else {
    cat("Fout bij ophalen geometrie", i, "met ID:", geo_id, "Status:", status_code(response), "\n")
  }
}

# Verzamel alle jsons met geometrie
geojson_files <- list.files(path = paste0(base_dir), pattern = paste0("^", document_id_output, "_\\d+\\.json$"), full.names = TRUE)
# Lijst om geometrieën op te slaan
geoms <- list()
# Loop door alle GeoJSON-bestanden en lees ze in als sf-objecten
for (f in geojson_files) {
  tryCatch({
    # Lees GeoJSON bestand in als sf-object
    sf_obj <- st_read(f, quiet = TRUE)
    
    # Check CRS of het bestand
    cat("Original CRS for file", f, ":", st_crs(sf_obj)$epsg, "\n")
    
    # Geometrie in EPSG:28992 (Amersfoort / RD New) is
    if (st_crs(sf_obj)$epsg == 4326) {
      cat("Overriding CRS to EPSG:28992 (Amersfoort / RD New).\n")
      st_crs(sf_obj) <- 28992
    }
    
    # Toevoegen van het sf-object aan de lijst
    geoms[[length(geoms) + 1]] <- sf_obj
  }, error = function(e) {
    cat("Kon bestand niet inlezen als sf:", f, "\n")
  })
}

#Creer sf bestand met meerder geometrien  
if (length(geoms) > 0) {
  all_geoms <- do.call(rbind, geoms)  
  gpkg_path <- file.path(base_dir, "/",  paste0(document_id_output, ".gpkg"))
  st_write(all_geoms, gpkg_path, delete_layer = TRUE)
  
  cat("Geopackage opgeslagen als:", gpkg_path, "\n")
} else {
  cat("Geen geldige geometrieën gevonden om te combineren.\n")
}