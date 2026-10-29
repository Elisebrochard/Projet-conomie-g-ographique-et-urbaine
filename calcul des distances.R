library(tidyverse)
library(sf)

# 1. Chargement de votre fichier DVF récent généré précédemment
base_dvf <- read_csv("dvf_maisons_bordeaux_ligneD_recent.csv")

# Filtrer les lignes sans coordonnées GPS
base_dvf_geo <- base_dvf %>%
  filter(!is.na(longitude), !is.na(latitude))

# 2. Conversion des maisons en objet spatial (WGS84 -> Lambert-93 pour calculs en MÈTRES)
maisons_sf <- st_as_sf(
  base_dvf_geo, 
  coords = c("longitude", "latitude"), 
  crs = 4326 # WGS84
) %>%
  st_transform(2154) # Projection officielle Lambert-93 (France)

# 3. Création directe de la table des stations de la Ligne D (Coordonnées GPS officielles)
stations_ligne_d <- tribble(
  ~nom_station, ~longitude, ~latitude,
  "Quinconces", -0.5739, 44.8442,
  "Tourny", -0.5768, 44.8436,
  "Fondaudège - Muséum", -0.5822, 44.8465,
  "Croix de Seguey", -0.5868, 44.8498,
  "Barrière du Médoc", -0.5925, 44.8540,
  "Courbet", -0.5968, 44.8569,
  "Calypso", -0.6015, 44.8601,
  "Mairie du Bouscat", -0.6083, 44.8643,
  "Les Écus", -0.6137, 44.8677,
  "Sainte-Germaine", -0.6189, 44.8710,
  "Champ de Course - Treulon", -0.6268, 44.8755,
  "Hippodrome", -0.6335, 44.8789,
  "Germignan", -0.6418, 44.8828,
  "Le Sulky", -0.6512, 44.8862,
  "Cantinolle", -0.6601, 44.8885
)

# Conversion des stations en objet spatial Lambert-93
stations_sf <- st_as_sf(
  stations_ligne_d, 
  coords = c("longitude", "latitude"), 
  crs = 4326
) %>%
  st_transform(2154)

# 4. Calcul de la matrice de distance (en MÈTRES) entre chaque maison et toutes les stations
matrice_distances <- st_distance(maisons_sf, stations_sf)

# Extraction de la distance minimale à la station la plus proche pour chaque maison
maisons_sf$dist_station_m <- apply(matrice_distances, 1, min)

# Création de tranches de distance (Buffer / Anneaux spatiaux pour l'économétrie)
maisons_sf <- maisons_sf %>%
  mutate(
    tranche_distance = case_when(
      dist_station_m <= 300 ~ "< 300m (Accès immédiat)",
      dist_station_m > 300 & dist_station_m <= 800 ~ "300m - 800m (Marche rapide)",
      dist_station_m > 800 & dist_station_m <= 1500 ~ "800m - 1500m (Zone d'influence)",
      TRUE ~ "> 1500m (Zone de contrôle)"
    )
  )

# 5. Conversion en dataframe classique et sauvegarde
base_finale <- maisons_sf %>%
  st_drop_geometry()

# Sauvegarde du jeu de données prêt pour la régression
write_csv(base_finale, "dvf_bordeaux_ligneD_avec_distances.csv")

message("Calcul des distances terminé avec succès !")