# Installation des packages nécessaires si ce n'est pas déjà fait :
# install.packages(c("tidyverse", "fixest"))

library(tidyverse)
library(fixest) # Le package de référence sous R pour les régressions avec effets fixes

# 1. Chargement de la base préparée à l'étape précédente
base_regression <- read_csv("dvf_bordeaux_ligneD_avec_distances.csv")

# 2. Nettoyage final pour l'économétrie (suppression des valeurs aberrantes ou manquantes)
base_clean <- base_regression %>%
  filter(
    !is.na(valeur_fonciere),
    !is.na(surface_reelle_bati),
    !is.na(dist_station_m),
    surface_reelle_bati > 15,          # Écarter les petites surfaces atypiques
    valeur_fonciere > 50000            # Écarter les cessions louches / pas de porte
  ) %>%
  mutate(
    prix_m2 = valeur_fonciere / surface_reelle_bati,
    ln_prix_m2 = log(prix_m2),
    ln_surface = log(surface_reelle_bati),
    # Transformation de la distance en centaines de mètres pour une lecture plus lisible du coefficient
    dist_station_hectometre = dist_station_m / 100 
  )

# 3. Estimation du modèle hédonique avec le package 'fixest' (fonction fe)
# On contrôle par :
# - Les caractéristiques physiques (log(surface), nombre de pièces)
# - La distance au tram (en hectomètres ou en mètres)
# - Des effets fixes par commune (code_commune) et par année (annee_mutation)

modele_hedonique <- feols(
  ln_prix_m2 ~ dist_station_hectometre + ln_surface + nombre_pieces_principales | code_commune + annee_mutation,
  data = base_clean
)

# 4. Affichage du résumé des résultats de la régression
summary(modele_hedonique)