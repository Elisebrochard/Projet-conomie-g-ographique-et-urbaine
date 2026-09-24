# =============================================================
# Exploration, cartographie et régression hédonique
# Projet éco géo urbaine - M2 ECAP
# La proximité au T9 est-elle associée à des prix plus élevés ?
# =============================================================

library(tidyverse)
library(sf)
library(ggplot2)

dvf_sf <- st_read("data_finales/dvf_t9.gpkg", quiet = TRUE)
stations_sf <- st_read("data_brutes/arrets-lignes.geojson", quiet = TRUE) %>%
  distinct(stop_name, .keep_all = TRUE) %>%
  st_transform(2154)

# -------------------------------------------------------------
# 1. VERIFICATIONS RAPIDES
# -------------------------------------------------------------
nrow(dvf_sf)
summary(dvf_sf$prix_m2)
table(dvf_sf$annee)
table(dvf_sf$traite)
summary(dvf_sf$dist_station_m)

# -------------------------------------------------------------
# 2. CARTE 1 : ZONE D'ETUDE (stations + transactions)
# -------------------------------------------------------------
ggplot() +
  geom_sf(data = dvf_sf, aes(color = factor(traite)), size = 0.5, alpha = 0.5) +
  geom_sf(data = stations_sf, shape = 17, size = 2, color = "black") +
  scale_color_manual(
    values = c("0" = "grey70", "1" = "firebrick"),
    labels = c("Hors zone (>800m)", "Zone proche (<=800m)"),
    name = NULL
  ) +
  labs(title = "Zone d'étude : transactions DVF et stations du T9") +
  theme_minimal()

ggsave("data_finales/carte_zone_etude.png", width = 8, height = 6, dpi = 300)

# -------------------------------------------------------------
# 3. CARTE 2 : PRIX AU M2 SELON LA DISTANCE AU T9
# -------------------------------------------------------------
ggplot(dvf_sf) +
  geom_sf(aes(color = prix_m2), size = 0.6, alpha = 0.7) +
  geom_sf(data = stations_sf, shape = 17, size = 2, color = "black") +
  scale_color_viridis_c(name = "Prix/m2") +
  labs(title = "Prix au m2 des transactions autour du T9 (2021-2023)") +
  theme_minimal()

ggsave("data_finales/carte_prix.png", width = 8, height = 6, dpi = 300)

# -------------------------------------------------------------
# 4. RELATION PRIX / DISTANCE (avant régression)
# -------------------------------------------------------------
ggplot(dvf_sf %>% st_drop_geometry(), aes(x = dist_station_m, y = prix_m2)) +
  geom_point(alpha = 0.2, size = 0.8) +
  geom_smooth(method = "loess", color = "firebrick") +
  labs(
    title = "Prix au m2 en fonction de la distance à la station T9 la plus proche",
    x = "Distance à la station (m)", y = "Prix au m2 (€)"
  ) +
  theme_minimal()

ggsave("data_finales/nuage_prix_distance.png", width = 8, height = 5, dpi = 300)

# -------------------------------------------------------------
# 5. REGRESSION HEDONIQUE EN COUPE
# -------------------------------------------------------------
# Spécification en distance continue (km), avec effets fixes année pour
# contrôler les tendances de marché sur 2021-2023
mod_continu <- lm(
  prix_m2 ~ dist_km + surface_reelle_bati + nombre_pieces_principales + factor(annee),
  data = dvf_sf
)
summary(mod_continu)

# Spécification alternative avec le seuil binaire (à comparer/discuter)
mod_seuil <- lm(
  prix_m2 ~ traite + surface_reelle_bati + nombre_pieces_principales + factor(annee),
  data = dvf_sf
)
summary(mod_seuil)

# Erreurs standards robustes (recommandé sur des prix immobiliers)
library(sandwich)
library(lmtest)
coeftest(mod_continu, vcov = vcovHC(mod_continu, type = "HC1"))
coeftest(mod_seuil,   vcov = vcovHC(mod_seuil,   type = "HC1"))

# -> Le coefficient d'intérêt est celui de dist_km (attendu négatif si la
#    proximité valorise le bien) ou celui de traite (attendu positif).
#    À discuter dans les limites : causalité non garantie (facteurs de
#    localisation non observés corrélés à la fois à la distance et au prix).