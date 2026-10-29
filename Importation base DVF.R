library(tidyverse)
library(lubridate)

# 1. Sélection des années
annees <- 2021:2024
dossier_donnees <- "./donnees_dvf_bordeaux/"
if (!dir.exists(dossier_donnees)) dir.create(dossier_donnees)

# 2. Fonction de téléchargement et nettoyage corrigée
telecharger_dvf_gironde <- function(annee) {
  url <- paste0("https://files.data.gouv.fr/geo-dvf/latest/csv/", annee, "/departements/33.csv.gz")
  fichier_local <- paste0(dossier_donnees, "dvf_33_", annee, ".csv.gz")
  
  if (!file.exists(fichier_local)) {
    message(sprintf("Téléchargement DVF 33 pour l'année %d...", annee))
    tryCatch({
      download.file(url, destfile = fichier_local, mode = "wb")
    }, error = function(e) {
      message(sprintf("Erreur année %d : %s", annee, e$message))
      return(NULL)
    })
  }
  
  if (!file.exists(fichier_local)) return(NULL)
  
  message(sprintf("Lecture et filtrage de l'année %d...", annee))
  df <- read_csv(fichier_local, show_col_types = FALSE)
  
  # Correction ici : 'surface_reelle_bati' prend bien deux 'e' à réelle
  df_clean <- df %>%
    filter(type_local == "Maison") %>%
    mutate(
      date_mutation = ymd(date_mutation),
      annee_mutation = annee,
      valeur_fonciere = as.numeric(valeur_fonciere),
      surface_reelle_bati = as.numeric(surface_reelle_bati),
      prix_m2 = valeur_fonciere / surface_reelle_bati
    ) %>%
    filter(
      !is.na(valeur_fonciere), 
      !is.na(surface_reelle_bati), 
      valeur_fonciere > 10000, 
      surface_reelle_bati > 9
    )
  
  return(df_clean)
}

# 3. Assemblage des années
base_gironde_maisons <- map_dfr(annees, ~telecharger_dvf_gironde(.x))

# 4. Filtrage sur Bordeaux et les communes de la Ligne D
# Bordeaux (33063), Le Bouscat (33069), Bruges (33075), Eysines (33162)
codes_communes_ligne_d <- c("33063", "33069", "33075", "33162")

base_bordeaux_ligne_d <- base_gironde_maisons %>%
  filter(code_commune %in% codes_communes_ligne_d)

# Aperçu
glimpse(base_bordeaux_ligne_d)

# Exportation
write_csv(base_bordeaux_ligne_d, "dvf_maisons_bordeaux_ligneD_recent.csv")
message("Base DVF récente générée avec succès !")