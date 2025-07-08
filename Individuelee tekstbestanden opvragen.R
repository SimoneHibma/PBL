library(httr)
library(jsonlite)
library(dplyr)
install.packages("pagedown")
library(pagedown)

#### INDIVIDUEEL ####
# ==== 1. Instellingen ====
#Identificatie
id <- "/akn/nl/act/gm0200/2025/omgevingsvisie"          #<---- HIER vanuit website toevoegen https://omgevingswet.overheid.nl/regels-op-de-kaart/
#voeg handmatig nieuwe indetifictaies op via https://www.plannenvoordeleefomgeving.nl/dashboard of met de api voor Overhheid.nl (verander api naar BM en omgevingsvisie)

# API-key
api_key <- ""       #<---- HIER opvragen, in 1/2 dagen  https://aandeslagmetdeomgevingswet.nl/ontwikkelaarsportaal/formulieren/api-key-aanvragen-0/
# Opslagpad
output_path <- "C:/Users/User/Documents/PBL/"       #<---- HIER PATH TO OUTPUT toevoegen

#Covert ID
#functie
convert_id_to_base_id <- function(id) {
  # Vervang / door _ 
  base_id <- paste0(gsub("/", "_", id))
  return(base_id)
}
# Omzetten naar base_id
base_id <- convert_id_to_base_id(id)
voorkomens_id <- base_id  

# Opslaan files
output_path_tekst <- paste0(output_path, base_id, ".pdf")   
output_path <- paste0(output_path, base_id, ".json")   



# Juiste API-URLs (voor ONTWERPregelingen)
base_url <- paste0("https://service.omgevingswet.overheid.nl/publiek/omgevingsdocumenten/api/presenteren/v7/regelingen/", base_id)
voorkomens_url <- paste0("https://service.omgevingswet.overheid.nl/publiek/omgevingsdocumenten/api/presenteren/v7/regelingen/", voorkomens_id, "/voorkomens")


# === 2. Ophalen ===
response_main <- GET(base_url, add_headers(`X-Api-Key` = api_key))
response_voorkomens <- GET(voorkomens_url, add_headers(`X-Api-Key` = api_key))

# === 3. Verwerken ===
if (status_code(response_main) == 200 && status_code(response_voorkomens) == 200) {
  
  main_text <- content(response_main, as = "text", encoding = "UTF-8")
  voorkomens_text <- content(response_voorkomens, as = "text", encoding = "UTF-8")
  
  main_data <- fromJSON(main_text, simplifyVector = TRUE)
  voorkomens_data <- fromJSON(voorkomens_text, simplifyVector = TRUE)
  
  main_data$voorkomens_data <- voorkomens_data
  
  write_json(main_data, output_path, pretty = TRUE, auto_unbox = TRUE)
  print(paste("JSON opgeslagen als:", output_path))
  
} else {
  print(paste("Fout bij ophalen. Status hoofd:", status_code(response_main),
              "- Voorkomens:", status_code(response_voorkomens)))
}

# Extraheer publicatieID dit is de link naar website waar tekst staat

if (file.exists(output_path)) {
  json_data <- fromJSON(output_path, simplifyVector = TRUE)
  identificatie <- json_data$identificatie
  publicatieID <- json_data$publicatieID
} else {
  print("JSON-bestand niet gevonden.")
}

#Tekst in pdf
chrome_print(input = publicatieID , output = output_path_tekst)
