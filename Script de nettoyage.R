# =============================================================
# Pipeline DVF (v4) : filtrage via DuckDB + variable spatiale
# Projet éco géo urbaine - M2 ECAP
# La proximité aux transports (T9) est-elle associée à des prix
# immobiliers plus élevés ? (régression hédonique en coupe, 2021-2023)
# =============================================================
# Le fichier DVF géolocalisées national contient déjà latitude/longitude
# (géocodage à la parcelle) -> pas besoin de géocoder via l'API BAN.
# On utilise DuckDB pour filtrer directement dans le csv.gz (sans tout
# charger en mémoire), sur les communes exactes traversées par le T9.
#
# NB : le géocodage de ce dataset repose sur le cadastre de juillet 2021,
# donc aucune transaction géolocalisée n'est disponible avant 2021 pour
# cette zone. D'où le choix d'une analyse en coupe (2021-2023 avec effets
# fixes année) plutôt qu'un avant/après.

library(tidyverse)
library(duckdb)
library(sf)

dir.create("data_brutes", showWarnings = FALSE)
dir.create("data_finales", showWarnings = FALSE)

# -------------------------------------------------------------
# 1. STATIONS T9 ET COMMUNES TRAVERSEES
# -------------------------------------------------------------
# Fichier GeoJSON déjà exporté depuis l'open data IDFM (19 stations, ligne T9,
# colonnes stop_name / stop_lon / stop_lat / nom_commune / code_insee)
stations_sf <- st_read("data_brutes/arrets-lignes.geojson", quiet = TRUE) %>%
  distinct(stop_name, .keep_all = TRUE) %>%
  st_transform(2154)  # Lambert-93, projection métrique pour la France

# Codes commune (INSEE) traversés -- le T9 passe par Paris (dép. 75) ET
# le Val-de-Marne (dép. 94), donc on filtre sur les codes commune exacts
# plutôt que sur un seul département
codes_insee_t9 <- stations_sf %>% st_drop_geometry() %>% pull(code_insee) %>% unique()
communes_t9    <- stations_sf %>% st_drop_geometry() %>% pull(nom_commune) %>% unique()

print(codes_insee_t9)  # vérifier la liste avant de lancer la requête DuckDB

# -------------------------------------------------------------
# 2. TELECHARGEMENT DU FICHIER DVF NATIONAL (une seule fois)
# -------------------------------------------------------------
url_national <- "https://www.data.gouv.fr/api/1/datasets/r/d7933994-2c66-4131-a4da-cf7cd18040a4"
dest_national <- "data_brutes/dvf_geolocalisees_national.csv.gz"

if (!file.exists(dest_national)) {
  download.file(url_national, dest_national, mode = "wb")
}

# -------------------------------------------------------------
# 3. FILTRAGE VIA DUCKDB (communes T9 + années + type de bien)
# -------------------------------------------------------------
con <- dbConnect(duckdb())

annee_min <- 2021  # pas de géocodage disponible avant (cf. note en tête de script)
annee_max <- 2023
codes_sql <- paste0("'", codes_insee_t9, "'", collapse = ", ")

requete <- glue::glue("
  SELECT
    id_mutation, date_mutation, nature_mutation,
    TRY_CAST(valeur_fonciere AS DOUBLE) AS valeur_fonciere,
    adresse_numero, adresse_nom_voie, code_postal, code_commune,
    nom_commune, code_departement, type_local,
    TRY_CAST(surface_reelle_bati AS DOUBLE) AS surface_reelle_bati,
    nombre_pieces_principales,
    TRY_CAST(longitude AS DOUBLE) AS longitude,
    TRY_CAST(latitude AS DOUBLE) AS latitude
  FROM read_csv_auto(
    '{dest_national}',
    compression = 'gzip',
    ignore_errors = true,
    types = {{
      'valeur_fonciere': 'VARCHAR',
      'surface_reelle_bati': 'VARCHAR',
      'longitude': 'VARCHAR',
      'latitude': 'VARCHAR',
      'code_commune': 'VARCHAR',
      'code_postal': 'VARCHAR'
    }}
  )
  WHERE code_commune IN ({codes_sql})
    AND EXTRACT(YEAR FROM date_mutation) BETWEEN {annee_min} AND {annee_max}
    AND nature_mutation = 'Vente'
    AND type_local IN ('Appartement', 'Maison')
    AND TRY_CAST(valeur_fonciere AS DOUBLE) IS NOT NULL
    AND TRY_CAST(surface_reelle_bati AS DOUBLE) > 0
")

dvf <- dbGetQuery(con, requete)
dbDisconnect(con, shutdown = TRUE)

# -------------------------------------------------------------
# 4. NETTOYAGE DU DVF
# -------------------------------------------------------------
# Important : une id_mutation peut regrouper plusieurs lots (ex. plusieurs
# appartements vendus en bloc), auquel cas valeur_fonciere est le prix TOTAL
# de la transaction, pas celui de la ligne. On ne garde que les mutations
# mono-lot pour que prix_m2 = valeur_fonciere / surface_reelle_bati soit fiable.
mutations_mono_lot <- dvf %>%
  count(id_mutation) %>%
  filter(n == 1) %>%
  pull(id_mutation)

dvf <- dvf %>%
  filter(id_mutation %in% mutations_mono_lot) %>%
  mutate(
    annee = as.integer(format(date_mutation, "%Y")),
    prix_m2 = valeur_fonciere / surface_reelle_bati
  ) %>%
  filter(
    !is.na(longitude), !is.na(latitude),
    prix_m2 > 500, prix_m2 < 20000   # bornes pour retirer les valeurs aberrantes
  )

# -------------------------------------------------------------
# 5. CONSTRUCTION DE LA VARIABLE SPATIALE : DISTANCE A LA STATION T9
# -------------------------------------------------------------
dvf_sf <- dvf %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(2154)

dist_matrix <- st_distance(dvf_sf, stations_sf)
dvf_sf$dist_station_m <- apply(dist_matrix, 1, min)

# Variable de traitement (seuil de proximité) + distance en continu (en km,
# pour une lecture plus naturelle des coefficients de régression)
dvf_sf <- dvf_sf %>%
  mutate(
    traite   = if_else(dist_station_m <= 800, 1, 0),
    dist_km  = dist_station_m / 1000
  )

# -------------------------------------------------------------
# 6. EXPORT
# -------------------------------------------------------------
st_write(dvf_sf, "data_finales/dvf_t9.gpkg", delete_dsn = TRUE)
write_csv(st_drop_geometry(dvf_sf), "data_finales/dvf_t9.csv")

# -> dvf_sf est prêt pour :
#    - cartographie : ggplot2 + geom_sf() (ou tmap)
#    - régression hédonique en coupe :
#      lm(prix_m2 ~ dist_km + surface_reelle_bati + factor(annee), data = dvf_sf)

