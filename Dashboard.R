install.packages("webshot")
webshot::install_phantomjs() 
install.packages("udpipe")
install.packages("pagedown")
install.packages("pdftools")
install.packages("jsonlite")
install.packages(c("tidytext", "stringr", "tibble"))
install.packages("RSelenium")
install.packages("rvest")
library(pdftools)
library(jsonlite)
library(pagedown)
library(jsonlite)
library(tidyr)
library(purrr)
library(tidytext)
library(stringr)
library(tibble)
library(rvest)
library(httr)
library(dplyr)
library(openxlsx)


#### GEOMETRIE OMGEVINGSLOKET ####
# Stap 1: basisinstellingen
regelingId <- "16b102cf-81ff-4c0a-bb27-0c911391f91d"
regelingId_ <- gsub("/", "_", regelingId)
naam_verordening <- "Omgevingsvisie"
APIKEY <- "963d9e5d-8b5d-46fd-af27-acfbe98211b7"  # aanvragen via: https://aandeslagmetdeomgevingswet.nl/ontwikkelaarsportaal/formulieren/api-key-dso-prod-omgeving-rp-aanvragen/
url_pres <- paste0("https://service.omgevingswet.overheid.nl/publiek/omgevingsdocumenten/api/presenteren/v8/regelingen/", regelingId_, "/regeltekstannotaties")
url_geo_base <- "https://service.omgevingswet.overheid.nl/publiek/omgevingsdocumenten/api/geometrieopvragen/v1/geometrieen/"
crs_param <- "?crs=http%3A%2F%2Fwww.opengis.net%2Fdef%2Fcrs%2FEPSG%2F0%2F28992"


# Stap 2: identificatiecodes van locaties opvragen
response <- GET(url_pres, add_headers(`x-api-key` = APIKEY))
if (status_code(response) == 200) {
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  locaties <- data$locaties
  df <- as.data.frame(locaties) %>% filter(locatieType == "Gebied")
} else {
  stop(paste("Fout bij ophalen locaties:", status_code(response)))
}
print(head(df))

# Stap 3: geojson per identificatie opvragen en opslaan
a <- 10000
output_folder <- "C:/Users/User/Documents/PBL"

for (i in seq_len(nrow(df))) {
  identificatie <- df[i, "identificatie"]
  naam <- df[i, "noemer"]
  naam <- gsub("[:/ ]", "_", naam)
  url_geo <- paste0(url_geo_base, identificatie, crs_param)
  pad <- file.path(output_folder, naam_verordening, paste0(a, naam, ".geojson"))
  
  geo_response <- GET(url_geo, add_headers(
    `x-api-key` = APIKEY,
    `accept` = "application/json",
    `Content-Type` = "application/json"
  ))
  
  if (status_code(geo_response) == 200) {
    dir.create(dirname(output_folder), showWarnings = FALSE, recursive = TRUE)
    write(content(geo_response, "text", encoding = "UTF-8"), file = pad)
    a <- a + 1
    cat("geschreven:", pad, "\n")
  } else {
    cat("Fout bij ophalen:", naam, "code:", status_code(geo_response), "\n")
  }
}

cat("---Einde---\n")

#### DOCUMENTEN & LINKS (na 2024) VIA DOCUMENT (DASHBOARD) ####
# JSON file from https://www.plannenvoordeleefomgeving.nl/stashboard
# Contact Michiel J. van Heek <michielvanheek@gmail.com> 
# Inlezen
json_data <- fromJSON("https://www.plannenvoordeleefomgeving.nl/regelingen/2025/04/2025-04-21_19:00.12m.json", flatten = TRUE)

# Functie om relevante velden eruit te halen
extract_omgevingsvisie <- function(data) {
  # Controleren of het een lijst is
  if (is.list(data)) {
    data <- bind_rows(data)
  }
  
  # Filteren op waarde "Omgevingsvisie"
  data %>%
    filter(type.waarde == "Omgevingsvisie") %>%
    mutate(
      naam = aangeleverdDoorEen.naam,
      code = aangeleverdDoorEen.code,
      versie = geregistreerdMet.versie
    ) %>%
    select(naam, code, officieleTitel, identificatie, technischId, versie, publicatieID)
}

# Gebruik de functie op je JSON
resultaat <- extract_omgevingsvisie(json_data)

# Print de tabel
View(resultaat)

# Link to JSON 
resultaat_met_link <- resultaat %>%
  mutate(
    # Clean the identification by keeping only up to the fifth "/"
    id_part1 = gsub("^(/[^/]+/[^/]+/[^/]+/[^/]+/[^/]+)(/.*)", "\\1", identificatie),
    
    # Extract the part after the fifth "/" 
    id_part2 = gsub("^/([^/]+/[^/]+/[^/]+/[^/]+/[^/]+/)(.*)", "\\2", identificatie),
    
    # Fix: Add underscore between "2024" and "omgevingsvisie"
    json_link = paste0("https://www.plannenvoordeleefomgeving.nl", 
                       id_part1, "/",
                       gsub("/", "_", id_part1), "_", id_part2, "%7C", versie, ".json")
  ) %>%
  select(naam, code, officieleTitel, identificatie, technischId, versie, publicatieID, json_link)


# View the final result with JSON links
View(resultaat_met_link)

# Houd voor elke identificatie alleen de hoogste versie
resultaat_met_link <- resultaat_met_link %>%
  mutate(
    is_ontwerp = str_detect(tolower(identificatie), "ontwerp"),
    # Maak een identifier zonder 'ontwerp' om te kunnen groeperen
    identificatie_basis = str_replace(tolower(identificatie), "ontwerp-", "")
  ) %>%
  group_by(identificatie_basis) %>%
  # Filter de hoogste versie binnen elke groep
  filter(versie == max(versie)) %>%
  # Verwijder 'ontwerp'-versie als er ook een definitieve versie is
  filter(!(is_ontwerp & any(!is_ontwerp))) %>%
  ungroup() %>%
  # Verwijder dubbele json_links
  filter(!duplicated(json_link)) %>%
  # Optioneel: kies kolommen die je wilt tonen
  select(naam, code, officieleTitel, identificatie, technischId, versie, publicatieID, json_link)


identificatie_df <- data.frame(
  identificatie = resultaat_met_link$identificatie,
  technischId = resultaat_met_link$technischId
)

# (optioneel) Exporteren naar Excel
library(openxlsx)
write.xlsx(identificatie_df, "C:/Users/User/Documents/PBL/identificaties_export_met_technischId.xlsx", rowNames = FALSE)


## GEO
# Vereiste packages
install.packages(c("httr", "jsonlite", "sf", "geojsonio"))  # Run alleen 1e keer
library(httr)
library(jsonlite)
library(sf)
library(geojsonio)

# Stap 1: Download JSON bestand
url <- "https://www.plannenvoordeleefomgeving.nl/akn/nl/act/gm0050/2024/_akn_nl_act_gm0050_2024_Regeling1af3515982e140cd9832fd9a4f2d69a8%7C1.json"
json_path <- "C:/Users/User/Documents/PBL/regeling.json"
GET(url, write_disk(json_path, overwrite = TRUE))

# Stap 2: Parse JSON bestand
data <- fromJSON(json_path, simplifyVector = FALSE)

# Stap 3: Zoek geometrieën (voorbeeld: uit 'gebiedsaanwijzingen' -> 'geometrie')
# Let op: Je moet de exacte structuur van het JSON-bestand verifiëren

extract_geometries <- function(json_data) {
  features <- list()
  entries <- json_data$gebiedsaanwijzingen
  
  for (entry in entries) {
    id <- entry$id
    geometry <- entry$geometrie$geometrie  # pas aan indien structuur anders is
    
    if (!is.null(geometry)) {
      coords <- matrix(unlist(geometry$coordinates), ncol = 2, byrow = TRUE)
      poly <- st_polygon(list(coords))
      sf_obj <- st_sf(id = id, geometry = st_sfc(poly), crs = 4326)
      features[[length(features) + 1]] <- sf_obj
    }
  }
  do.call(rbind, features)
}

# Stap 4: Maak een sf-object van de geometrieën
geometrie_sf <- extract_geometries(data)

# Stap 5: Exporteren naar GeoJSON
geojson_path <- "C:/Users/User/Documents/PBL/regeling_export.geojson"
geojsonio::geojson_write(geometrie_sf, file = geojson_path)

cat("✅ GeoJSON opgeslagen naar:", geojson_path)



# Stap 1: Download JSON bestand
url <- "https://www.plannenvoordeleefomgeving.nl/akn/nl/act/gm0050/2024/_akn_nl_act_gm0050_2024_Regeling1af3515982e140cd9832fd9a4f2d69a8%7C1.json"
json_path <- "C:/Users/User/Documents/PBL/regeling.json"
GET(url, write_disk(json_path, overwrite = TRUE))

# Stap 2: Parse JSON bestand
data <- fromJSON(json_path, simplifyVector = FALSE)

# Stap 3: Zoek geometrieën (voorbeeld: uit 'gebiedsaanwijzingen' -> 'geometrie')
# Let op: Je moet de exacte structuur van het JSON-bestand verifiëren

extract_geometries <- function(json_data) {
  features <- list()
  entries <- json_data$gebiedsaanwijzingen
  
  for (entry in entries) {
    id <- entry$id
    geometry <- entry$geometrie$geometrie  # pas aan indien structuur anders is
    
    if (!is.null(geometry)) {
      coords <- matrix(unlist(geometry$coordinates), ncol = 2, byrow = TRUE)
      poly <- st_polygon(list(coords))
      sf_obj <- st_sf(id = id, geometry = st_sfc(poly), crs = 4326)
      features[[length(features) + 1]] <- sf_obj
    }
  }
  do.call(rbind, features)
}

# Stap 4: Maak een sf-object van de geometrieën
geometrie_sf <- extract_geometries(data)

# Stap 5: Exporteren naar GeoJSON
geojson_path <- "C:/Users/User/Documents/PBL/regeling_export.geojson"
geojsonio::geojson_write(geometrie_sf, file = geojson_path)

cat("✅ GeoJSON opgeslagen naar:", geojson_path)







