# Alle omgevingsvisie bestanden aanroepen die op dit moment beschikbaar zijn
# Er is ook een code om individuele documenten op te halen
# voeg handmatig nieuwe indetifictaies toe aan het bestand alle_identificaties_omgevingsvisies
# zoek nieuwe op via https://www.plannenvoordeleefomgeving.nl/dashboard 
# of met de api voor Overhheid.nl (verander api link naar "BM" (typedocument) en "omgevingsvisie" (zoekterm))
#functies in R
install.packages(c("httr", "jsonlite", "openxlsx", "dplyr", "rvest", "tidyr"))
invisible(lapply(c("httr", "jsonlite", "openxlsx", "dplyr", "rvest", "tidyr"), library, character.only = TRUE))

# ==== 1. Inlezen identificaties ====
identificatie_df <- read.xlsx("C:/Users/User/Documents/PBL/alle_identificaties_omgevingsvisies.xlsx") #zelf link naar gedownloaden bestand maken

# ==== 2. Instellingen ====
api_key <- ""  #<---- HIER opvragen, in 1/2 dagen  https://aandeslagmetdeomgevingswet.nl/ontwikkelaarsportaal/formulieren/api-key-aanvragen-0/
output_dir_tussenstap <- "C:/Users/User/Documents/PBL/json_output"  #zelf link naar bestaande map maken
output_dir <- "C:/Users/User/Documents/PBL/Text_documenten"         #zelf link naar bestaande map maken

if (!dir.exists(output_dir)) dir.create(output_dir)

# ==== 3. Itereer over rijen ====
for (i in seq_len(nrow(identificatie_df))) {
  
  identificatie <- identificatie_df$identificatie[i]
  technisch_id <- identificatie_df$technischId[i]
  
  print(paste("🔄 Bezig met:", identificatie))
  success <- FALSE
  
  # === Eerst proberen via REGELINGEN ===
  base_url <- paste0("https://service.omgevingswet.overheid.nl/publiek/omgevingsdocumenten/api/presenteren/v7/regelingen/", identificatie)
  voorkomens_url <- paste0("https://service.omgevingswet.overheid.nl/publiek/omgevingsdocumenten/api/presenteren/v7/regelingen/", identificatie, "/voorkomens")
  
  response_main <- GET(base_url, add_headers(`X-Api-Key` = api_key))
  response_voorkomens <- GET(voorkomens_url, add_headers(`X-Api-Key` = api_key))
  
  if (status_code(response_main) == 200 && status_code(response_voorkomens) == 200) {
    
    main_text <- content(response_main, as = "text", encoding = "UTF-8")
    voorkomens_text <- content(response_voorkomens, as = "text", encoding = "UTF-8")
    
    main_data <- fromJSON(main_text, simplifyVector = TRUE)
    voorkomens_data <- fromJSON(voorkomens_text, simplifyVector = TRUE)
    
    main_data$voorkomens_data <- voorkomens_data
    main_data$bron_type <- "regeling"
    
    safe_name <- gsub("[^a-zA-Z0-9]", "_", identificatie)
    output_file <- file.path(output_dir, paste0(safe_name, ".json"))
    
    write_json(main_data, output_file, pretty = TRUE, auto_unbox = TRUE)
    print(paste("✅ Opgeslagen als REGELING:", output_file))
    success <- TRUE
    
  } else {
    # === Daarna proberen via ONTWERPREGELINGEN ===
    base_url <- paste0("https://service.omgevingswet.overheid.nl/publiek/omgevingsdocumenten/api/presenteren/v7/ontwerpregelingen/", technisch_id)
    
    response_main <- GET(base_url, add_headers(`X-Api-Key` = api_key))
    
    if (status_code(response_main) == 200) {
      main_text <- content(response_main, as = "text", encoding = "UTF-8")
      main_data <- fromJSON(main_text, simplifyVector = TRUE)
      
      main_data$voorkomens_data <- NULL  # Niet opvragen, zit al in base
      main_data$bron_type <- "ontwerpregeling"
      
      safe_name <- gsub("[^a-zA-Z0-9]", "_", technisch_id)
      output_file <- file.path(output_dir, paste0(safe_name, ".json"))
      
      write_json(main_data, output_file, pretty = TRUE, auto_unbox = TRUE)
      print(paste("✅ Opgeslagen als ONTWERPREGELING:", output_file))
      success <- TRUE
    }
  }
  
  if (!success) {
    print(paste("❌ Mislukt voor:", identificatie, "(en technischId:", technisch_id, ")"))
  }
}

#ophalen json met basis informatie zodat het in een dataframe samenkomt 
json_files <- list.files(output_dir_tussenstap, pattern = "\\.json$", full.names = TRUE)

# Initialiseer een lijst voor resultaten
results <- list()

for (file in json_files) {
  json_data <- fromJSON(file)
  
  # Haal identificatie en publicatieID op
  identificatie <- json_data$identificatie
  publicatieID <- json_data$publicatieID
  
  # Voeg toe aan resultaten
  results[[length(results) + 1]] <- data.frame(
    naam = json_data$aangeleverdDoorEen$naam,
    code = json_data$aangeleverdDoorEen$code,
    officieleTitel = json_data$officieleTitel,
    identificatie = json_data$identificatie,
    publicatieID = publicatieID,
    versie = json_data$geregistreerdMet$versie,
    stringsAsFactors = FALSE
  )
}

# Combineer alle rijen tot een data.frame
result_df <- bind_rows(results)

#json link toevoegen met tekst en metadata, werkt niet voor plannen met technischID want dit is nog ontwerp
result_df <- result_df%>%
  mutate(
    identificatie_opslaan = gsub("/", "_", identificatie),
    # Schoon de identificatie op door alleen het eerste deel te behouden
    id_part1 = gsub("^(/[^/]+/[^/]+/[^/]+/[^/]+/[^/]+)(/.*)", "\\1", identificatie),
    
    # Deel na de vijfde "/" extraheren
    id_part2 = gsub("^/([^/]+/[^/]+/[^/]+/[^/]+/[^/]+/)(.*)", "\\2", identificatie),
    
    # Voeg underscore tussen "2024" en "omgevingsvisie" toe
    json_link = paste0("https://www.plannenvoordeleefomgeving.nl", 
                       id_part1, "/",
                       gsub("/", "_", id_part1), "_", id_part2, "%7C", versie, ".json")
  ) %>%
  select(naam, code, officieleTitel, identificatie, versie, publicatieID, json_link, identificatie_opslaan)

#Text bestanden vanuit df opvragen website
for (i in 1:nrow(result_df)) {
  url <- result_df$publicatieID[i]
  
  # Gebruik de index 'i' voor de naam van het bestand
  id_opslaan <- result_df$identificatie_opslaan[i]
  
  # Bepaal het pad voor de PDF
  output_path <- file.path(output_dir, paste0(id_opslaan, ".pdf"))
  
  # Zet de webpagina om naar PDF
  chrome_print(input = url, output = output_path)
  cat("PDF opgeslagen als:", output_path, "\n")
}

#Json files met tekst en metadata vanuit df opvragen vanuit link naar ruimtelijkplannen dashboard 
for (i in 1:nrow(result_df)) {
  # Haal de json_link op
  json_link <- result_df$json_link[i]
  
  # Gebruik de index 'i' voor de naam van het bestand
  id_opslaan <- result_df$identificatie_opslaan[i]
  
  # Bepaal het pad voor het JSON-bestand
  output_path_json <- file.path(output_dir, paste0(id_opslaan, ".json"))
  
  # Haal de JSON data op van de link en sla deze op
  tryCatch({
    # Als json_link een URL is, kun je de inhoud van de link ophalen
    json_data <- fromJSON(json_link)  # Veronderstelt dat het een geldige JSON URL is
    
    # Sla de JSON data op
    write_json(json_data, output_path_json)
    cat("JSON opgeslagen als:", output_path_json, "\n")
  }, error = function(e) {
    cat("Fout bij het ophalen van JSON voor:", id_opslaan, "\n")
    cat("Foutmelding:", e$message, "\n")
  })
}
