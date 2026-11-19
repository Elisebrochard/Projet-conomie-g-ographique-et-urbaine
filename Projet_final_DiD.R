# ==============================================================================
# PROJET D'ÉCONOMIE GÉOGRAPHIQUE ET URBAINE APPLIQUÉE
# Impact de la Ligne D du tramway bordelais sur les prix immobiliers
# ==============================================================================

library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(fixest)
library(scales)
library(stringi)
library(geodata)
library(mapview)
library(ggspatial)

# ------------------------------------------------------------------------------
# 0. CHARGEMENT DES DONNÉES (VERSION GEOPACKAGE .gpkg)
# ------------------------------------------------------------------------------

path_dvf <- "dvf_plus_d33/dvf_plus_d33.gpkg"
path_stations <- "sv_arret_p.csv" 

# Lecture directe du GeoPackage avec sf (plus besoin d'open_dataset)
dvf_brut <- st_read(path_dvf)

# Si ton fichier stations est un CSV avec des coordonnées :
stations <- read.csv2(path_stations, row.names = NULL)


# ------------------------------------------------------------------------------
# 1. PRÉPARATION ET NETTOYAGE DES DONNÉES (DVF+)
# ------------------------------------------------------------------------------

dvf_bordeaux <- dvf_brut |>
  mutate(codinsee = as.character(l_codinsee)) |>
  filter(
    codinsee %in% c("33063", "33069", "33075", "33162"), # Bordeaux, Le Bouscat, Bruges, Eysines
    nbcomm == 1,
    libnatmut == "Vente",
    libtypbien %in% c("UNE MAISON", "UN APPARTEMENT"),
    nblocmai + nblocapt == 1,
    !is.na(sbati), sbati > 20,
    !is.na(valeurfonc), valeurfonc > 0
  ) |>
  st_transform(2154) |> # Passage en Lambert 93
  filter(!st_is_empty(geom)) |>
  mutate(
    appartement = ifelse(nblocapt > 0, 1, 0),
    maison = ifelse(nblocmai > 0, 1, 0),
    # Reconstruction des pièces (méthode TD)[cite: 5]
    nb_pieces_appart = 1 * nbapt1pp + 2 * nbapt2pp + 3 * nbapt3pp + 4 * nbapt4pp + 5 * nbapt5pp,
    nb_pieces_maison = 1 * nbmai1pp + 2 * nbmai2pp + 3 * nbmai3pp + 4 * nbmai4pp + 5 * nbmai5pp,
    nb_pieces_total = nb_pieces_appart + nb_pieces_maison,
    prix = valeurfonc,
    surface = sbati,
    surf_terrain = coalesce(sterr, 0),
    prix_m2 = prix / surface,
    annee = anneemut,
    mois = moismut,
    quartier = paste(codinsee, l_section, sep = "_")
  ) |>
  filter(is.finite(prix_m2), prix_m2 > 500, prix_m2 < 15000)

# Point représentatif dans la parcelle (st_point_on_surface du TD)[cite: 5, 8]
dvf_points <- dvf_bordeaux |>
  mutate(geometry = st_point_on_surface(geom))


# ------------------------------------------------------------------------------
# 2. VARIABLE SPATIALE : DISTANCE À LA LIGNE D
# ------------------------------------------------------------------------------

stations_tram <- stations |> 
  dplyr::filter(VEHICULE == "TRAM") |>
  dplyr::mutate(nom = tolower(stri_trans_general(LIBELLE, "Latin-ASCII"))) |>
  separate(`Geo.Point`, into = c("lat", "lon"), sep = ",\\s*", convert = TRUE) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(2154)

ligne_d_propre <- c(
  "fondaudege", "croix de seguey", "barriere du medoc", "courbet",
  "calypso", "mairie du bouscat", "ecus", "sainte.germaine",
  "courses", "hippodrome", "simone veil", "picot",
  "eysines centre", "les sources", "cantinolle"
)

stations_ligne_d <- stations_tram |>
  filter(grepl(paste(ligne_d_propre, collapse = "|"), nom))

matrice_dist <- st_distance(dvf_points, stations_ligne_d)
dvf_points$dist_ligne_d <- apply(matrice_dist, 1, min)

# ----------------------------------------------------------------
# 2. bis. Stats desc
# ----------------------------------------------------------------

# On enlève la géométrie pour travailler sur un simple tableau
desc <- dvf_points |> st_drop_geometry()
desc$dist_ligne_d <- as.numeric(desc$dist_ligne_d)

# Nombre d'observations
nrow(desc)

# Résumé des variables principales
summary(desc[, c("prix_m2", "surface", "nb_pieces_total", "maison", "surf_terrain", "dist_ligne_d")])

# Distribution du prix au m² (valeurs aberrantes ?)
boxplot(desc$prix_m2, main = "Prix au m2", ylab = "euros/m2", col = "red4")
hist(desc$prix_m2, breaks = 50, main = "Distribution du prix au m2", xlab = "euros/m2", col = "red4")

# Nombre de ventes par année (il faut des ventes avant 2019)
table(desc$annee)

# Nombre de ventes par commune
table(desc$codinsee)

# Part de maisons / appartements
table(desc$maison)

# Distribution de la distance à la station la plus proche
hist(desc$dist_ligne_d, breaks = 50, main = "Distance à la station la plus proche", xlab = "mètres", col = "brown")

# Combien de logements dans chaque zone (avant de définir les groupes)
table(cut(desc$dist_ligne_d, breaks = c(0, 500, 1000, Inf),
          labels = c("< 500 m (traité)", "500-1000 m (exclu)", "> 1000 m (contrôle)"),
          right = FALSE))

# ------------------------------------------------------------------------------
# 3. CARTES INTERACTIVES (`mapview`, style TD)
# ------------------------------------------------------------------------------

carte_data_2023 <- dvf_points |>
  filter(annee == 2023, !is.na(dist_ligne_d)) |>
  filter(dist_ligne_d < 500 | dist_ligne_d > 1000) |>
  mutate(groupe = ifelse(dist_ligne_d < 500, "Traité (< 500m)", "Contrôle (> 1000m)"))

# S'ouvre dans l'onglet Viewer de RStudio
mapview(carte_data_2023, zcol = "groupe", col.regions = c("grey60", "red"), 
        cex = 3, layer.name = "Groupe DiD", map.types = "OpenStreetMap") +
  mapview(stations_ligne_d, col.regions = "black", cex = 5, layer.name = "Stations Ligne D")


# ------------------------------------------------------------------------------
# 4. CARTES STATIQUES QUALITATIVES (`ggplot2` + Fond OpenStreetMap)
# ------------------------------------------------------------------------------
### NE FONCTIONNE PAS 
carte_prix_osm <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 12) +
  geom_sf(data = carte_data_2023, aes(color = prix_m2), size = 0.9, alpha = 0.8) +
  geom_sf(data = stations_ligne_d, color = "black", shape = 17, size = 2.5) +
  scale_color_viridis_c(
    option = "magma",
    direction = -1,
    name = "Prix au m² (€)",
    labels = label_number(big.mark = " ", suffix = " €")
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Distribution spatiale des prix au m² autour de la Ligne D (2023)",
    subtitle = "Bordeaux, Le Bouscat, Bruges et Eysines",
    caption = "Sources : DVF+ (Cerema), Open Data Bordeaux Métropole, OpenStreetMap."
  ) +
  theme_void()

print(carte_prix_osm)


# ------------------------------------------------------------------------------
# 5. ANALYSE ÉCONOMÉTRIQUE (DOUBLE DIFFÉRENCE)
# ------------------------------------------------------------------------------

base_did <- dvf_points |>
  st_drop_geometry() |>
  filter(!is.na(dist_ligne_d), !is.na(nb_pieces_total)) |>
  filter(dist_ligne_d < 500 | dist_ligne_d > 1000) |>
  mutate(datemut = as.Date(datemut)) |>
  filter(datemut < as.Date("2019-12-14") | datemut > as.Date("2020-02-29")) |>
  mutate(
    traite = ifelse(dist_ligne_d < 500, 1, 0),
    post = ifelse(datemut > as.Date("2020-02-29"), 1, 0)
  )

m3 <- feols(log(prix_m2) ~ traite + traite:post + 
              log(surface) + nb_pieces_total + maison + asinh(surf_terrain) | quartier + annee^mois, 
            data = base_did, 
            vcov = ~quartier)

etable(m3, digits = 3,
       dict = c("traite:post" = "Effet Tram (DiD)",
                "log(surface)" = "Log(Surface)",
                "nb_pieces_total" = "Nombre de pièces",
                "maison" = "Maison",
                "asinh(surf_terrain)" = "asinh(Terrain)"))


#----------------------------------------------------------------------------
# 1. Création de la base complète (sans filtre sur dist_ligne_d)
base_complete <- dvf_points |>
  st_drop_geometry() |>
  filter(!is.na(dist_ligne_d), !is.na(nb_pieces_total)) |>
  mutate(
    datemut = as.Date(datemut),
    dist_ligne_d = as.numeric(dist_ligne_d),
    post = ifelse(datemut > as.Date("2020-02-29"), 1, 0)
  ) |>
  filter(datemut < as.Date("2019-12-14") | datemut > as.Date("2020-02-29"))

# 2. Préparation des bases avec différents seuils
# Modèle de référence (< 500m vs > 1000m)
base_500 <- base_complete |> 
  filter(dist_ligne_d < 500 | dist_ligne_d > 1000) |> 
  mutate(traite = ifelse(dist_ligne_d < 500, 1, 0))

# Test de robustesse 1 : Proximité immédiate (< 300m vs > 1000m)
base_300 <- base_complete |> 
  filter(dist_ligne_d < 300 | dist_ligne_d > 1000) |> 
  mutate(traite = ifelse(dist_ligne_d < 300, 1, 0))

# Test de robustesse 2 : Périmètre élargi (< 750m vs > 1000m)
base_750 <- base_complete |> 
  filter(dist_ligne_d < 750 | dist_ligne_d > 1000) |> 
  mutate(traite = ifelse(dist_ligne_d < 750, 1, 0))

# 3. Estimation des modèles DiD binaires
m_ref_500 <- feols(
  log(prix_m2) ~ traite + traite:post + log(surface) + nb_pieces_total + maison + asinh(surf_terrain) | 
    quartier + annee^mois,
  data = base_500,
  vcov = ~quartier
)

m_rob_300 <- feols(
  log(prix_m2) ~ traite + traite:post + log(surface) + nb_pieces_total + maison + asinh(surf_terrain) | 
    quartier + annee^mois,
  data = base_300,
  vcov = ~quartier
)

m_rob_750 <- feols(
  log(prix_m2) ~ traite + traite:post + log(surface) + nb_pieces_total + maison + asinh(surf_terrain) | 
    quartier + annee^mois,
  data = base_750,
  vcov = ~quartier
)

# 4. Estimation du modèle continu (sur TOUTE la base, sans zone tampon)
m_continu <- feols(
  log(prix_m2) ~ log(dist_ligne_d + 1) + post:log(dist_ligne_d + 1) + 
    log(surface) + nb_pieces_total + maison + asinh(surf_terrain) | 
    quartier + annee^mois,
  data = base_complete,
  vcov = ~quartier
)

# 5. Affichage comparatif propre de tous les modèles
etable(
  list("Ref (<500m)" = m_ref_500, 
       "Proche (<300m)" = m_rob_300, 
       "Élargi (<750m)" = m_rob_750, 
       "Continu (Log Dist)" = m_continu),
  dict = c(
    "traite:post" = "Effet Tram (DiD Binaire)",
    "log(dist_ligne_d + 1)" = "Log(Distance)",
    "post:log(dist_ligne_d + 1)" = "Effet Tram (DiD Continu)",
    "log(surface)" = "Log(Surface)",
    "nb_pieces_total" = "Nombre de pièces",
    "maison" = "Maison",
    "asinh(surf_terrain)" = "asinh(Terrain)"
  ),
  se.below = TRUE,
  fitstat = ~ n + r2 + wr2
)

#-------------------------------------------------------------------------------







# ------------------------------------------------------------------------------
# 6. TENDANCES PARALLÈLES ET EVENT STUDY
# ------------------------------------------------------------------------------

tendances <- base_did |>
  group_by(annee, traite) |>
  summarise(prix_moyen = mean(prix_m2, na.rm = TRUE), .groups = "drop")

ggplot(tendances, aes(x = annee, y = prix_moyen, 
                      color = factor(traite, labels = c("Contrôle (> 1 000 m)", "Traité (< 500 m)")))) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 2019.9, linetype = "dashed", color = "grey30") +
  scale_y_continuous(labels = label_number(big.mark = " ", suffix = " €")) +
  labs(
    title = "Évolution du prix moyen au m² avant et après la Ligne D",
    subtitle = "Vérification graphique de l'hypothèse des tendances parallèles",
    x = "Année de transaction",
    y = "Prix moyen au m² (€)",
    color = "Groupe"
  ) +
  theme_minimal()

modele_es <- feols(
  log(prix_m2) ~ traite + i(annee, traite, ref = 2018) + 
    log(surface) + nb_pieces_total + maison + asinh(surf_terrain) | quartier + annee^mois, 
  data = base_did, 
  vcov = ~quartier
)

iplot(modele_es,
      main = "Event Study : Écart de prix Traité vs Contrôle (Réf. : 2018)",
      xlab = "Année",
      ylab = "Coefficient estimé (Log Prix/m²)")
abline(v = 2019.5, lty = 2, col = "red")



# Vérification rapide des tendances parallèles pour < 300 m
tendances_300 <- base_300 |>
  group_by(annee, traite) |>
  summarise(prix_moyen = mean(prix_m2, na.rm = TRUE), .groups = "drop")

ggplot(tendances_300, aes(x = annee, y = prix_moyen,
                          color = factor(traite, labels = c("Contrôle (> 1 000 m)", "Traité (< 300 m)")))) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 2019.9, linetype = "dashed", color = "grey30") +
  scale_y_continuous(labels = label_number(big.mark = " ", suffix = " €")) +
  labs(
    title = "Tendances parallèles : Traité (< 300 m) vs Contrôle (> 1 000 m)",
    x = "Année de transaction",
    y = "Prix moyen au m² (€)",
    color = "Groupe"
  ) +
  theme_minimal()
