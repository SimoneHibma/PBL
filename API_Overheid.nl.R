# Installeer de benodigde packages 
install.packages(c("httr", "jsonlite", "xml2", "dplyr"))
library(httr)
library(jsonlite)
library(xml2)
library(dplyr)

# ==== 1. Instellingen ====
 
# uitleg API/web service https://puc.overheid.nl/koop/doc/PUC_234678_13/1/

# Stel de URL samen van de API
endpoint <- "http://zoekdienst.overheid.nl/sru/Search?version=1.2&operation=searchRetrieve&x-connection="
#kies uit een van de volgende
# Lokale regelingen     = cvdr
# Lokale bekendmakingen = bm 
# Lokale vergunningen   = vg
collectie <- "cvdr"
aantal <- "100"
zoekterm <- "klimaatadaptatie"
url <- paste0("http://zoekdienst.overheid.nl/sru/Search?version=1.2&operation=searchRetrieve&x-connection=", 
              collectie, "&startRecord=1&maximumRecords=", aantal, "&query=title=", zoekterm)

#Instellingen
output_path <- "C:/Users/User/Documents/PBL/"             #zelf link naar bestaande map maken
naam_bestand <- "klimaatadaptatie_data"                   #naam kiezen, dit is niet uit resultaat
output_json <- paste0(output_path, naam_bestand, ".json")


# ==== 2. Opvragen met API ====
# Voer de GET-aanroep uit
response <- GET(url)

# Controleer de status van de response
if(status_code(response) == 200) {
  # Parse de inhoud van de response als JSON
  content <- content(response, "text")
  
  # Sla de inhoud op als een JSON-bestand
  write(content, file = output_json)
  
  print(paste("De data is succesvol opgeslagen in:", output_path))
} else {
  print("Er is een fout opgetreden bij het ophalen van de data.")
}

# ==== 3. Van json-file met alle documenten binnen API naar een tabel ====

## Informatie extraheren
# Lees de XML in
xml_data <- read_xml(output_json)

# Definieer de namespaces
ns <- xml_ns(xml_data)

# Zoek alle <meta>-elementen
meta_nodes <- xml_find_all(xml_data, ".//overheidrg:meta", ns)

# Parseer elke <meta> en haal gemeente en resourceIdentifier op (andere data kan ook in overzicht worden opgenomen)
result_list <- lapply(meta_nodes, function(node) {
  gemeente <- xml_text(xml_find_first(node, ".//dcterms:creator", ns))
  resource_id <- xml_attr(xml_find_first(node, ".//dcterms:isFormatOf", ns), "resourceIdentifier")
  
  if (!is.na(gemeente) && !is.na(resource_id)) {
    return(data.frame(gemeente = gemeente, resourceIdentifier = resource_id, stringsAsFactors = FALSE))  
  } else {
    return(NULL)
  }
})

# Combineer in één data.frame
Overzicht_gemeenten_en_link_naar_bestand <- bind_rows(result_list)

# ==== 3. Van Table naar individuele PDFs ====

Overzicht_gemeenten_en_link_naar_bestand$pdf_link <- ifelse(
  Overzicht_gemeenten_en_link_naar_bestand$resourceIdentifier != "",
  paste0(Overzicht_gemeenten_en_link_naar_bestand$resourceIdentifier, ".pdf"),
  ""
)
# Zorg dat de map bestaat
dir.create("pdfs", showWarnings = FALSE)              # naam map kan verandert worden

# Werk met kopie van originele data
df <- Overzicht_gemeenten_en_link_naar_bestand

# Filter alleen rijen met geldige links
df <- df[df$resourceIdentifier != "" & grepl("^https?://", df$resourceIdentifier), ]

# Voeg de pdf-link toe
df$pdf_link <- paste0(df$resourceIdentifier, ".pdf")

# Loop over elke rij
for (i in 1:nrow(df)) {
  
  # Maak veilige bestandsnaam
  gemeente_clean <- gsub("[^a-zA-Z0-9]", "_", df$gemeente[i])
  link_clean <- gsub("[^a-zA-Z0-9]", "_", basename(df$resourceIdentifier[i]))
  filename <- paste0("pdfs/", gemeente_clean, "_", link_clean, ".pdf")
  
  # Download met foutafhandeling
  tryCatch({
    download.file(df$pdf_link[i], destfile = filename, mode = "wb", quiet = TRUE)
    message(paste("✔ Gedownload:", filename))
  }, error = function(e) {
    message(paste("✖ Fout bij:", df$pdf_link[i]))
  })
}