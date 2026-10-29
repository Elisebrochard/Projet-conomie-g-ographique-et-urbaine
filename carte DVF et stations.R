# Installation des packages si nécessaire :
# install.packages(c("tidyverse", "sf", "viridis"))

library(tidyverse)
library(sf)
library(viridis)

# 1. Chargement de la base DVF récente (qui contient longitude et latitude)
base_dvf <- read_csv("dvf_maisons_bordeaux_ligneD_recent.csv")

# 2. Filtrage des données valides et conversion en objet spatial (Lambert-93)
maisons_sf <- base_dvf %>%
  filter(!is.na(longitude), !is.na(latitude), !is.na(prix_m2), prix_m2 < 10000) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(2154) # Lambert-93 pour la France

# 3. Définition des stations de la Ligne D
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

stations_sf <- stations_ligne_d %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(2154)

# 4. Création de la carte avec ggplot2
carte_bordeaux <- ggplot() +
  # Points des maisons colorés selon le prix au m²
  geom_sf(data = maisons_sf, aes(color = prix_m2), size = 1.2, alpha = 0.7) +
  scale_color_viridis_c(option = "plasma", name = "Prix au m² (€)") +
  
  # Stations de la Ligne D (triangles rouges)
  geom_sf(data = stations_sf, color = "red", shape = 17, size = 3.5) +
  
  # Habillage
  theme_minimal() +
  labs(
    title = "Valeur foncière des maisons et stations de la Ligne D (Bordeaux)",
    subtitle = "Points : Transactions immobilières (DVF) | Triangles rouges : Stations de tram",
    caption = "Sources : DVF & Open Data Bordeaux Métropole"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "right"
  )

# Affichage dans RStudio
print(carte_bordeaux)

# Sauvegarde de l'image
ggsave("carte_immobilier_tram_bordeaux.png", plot = carte_bordeaux, width = 10, height = 8, dpi = 300)
message("Carte générée et sauvegardée avec succès !")