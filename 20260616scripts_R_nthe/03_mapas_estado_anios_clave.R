# ============================================================================
# 03_mapas_estado_anios_clave.R
# Proyecto: Trayectoria de la superficie sembrada en Querétaro (1981-2024)
# ----------------------------------------------------------------------------
# OBJETIVO: Cuatro siluetas del estado de Querétaro (1981, 1993, 2014, 2024),
#           cada una coloreada según la superficie sembrada ESTATAL de ese año.
#           Debajo de cada mapa, la etiqueta del cambio porcentual respecto a
#           1981. Imagen apaisada (de izquierda a derecha de la hoja).
#
# NOTA: El color representa un valor ESTATAL único por año (no municipal); el
#       estado se dibuja como un solo polígono. Los datos son los mismos de la
#       Figura 1 (serie estatal). Los años son el inicio (1981), los quiebres
#       estructurales (1993, 2014) y el último año disponible (2024).
#
# INPUT:  serie_estatal_qro_1981-2024.csv   (salida de 01_limpieza_estatal.R)
# OUTPUT: fig_mapas_estado_anios_clave.png
#
# Requiere: mxmaps (geometría), sf (disolver municipios en silueta estatal).
#   install.packages("remotes"); remotes::install_github("diegovalle/mxmaps")
#
# Licencia código: MIT.
# ============================================================================

library(tidyverse)
library(mxmaps)
library(sf)
library(scales)

# --- DIRECTORIO DE TRABAJO ---------------------------------------------------
# Este script asume que se ejecuta desde la carpeta del proyecto. En RStudio,
# abrir el proyecto (.Rproj) o usar Session > Set Working Directory >
# To Source File Location.

# --- PARÁMETROS -------------------------------------------------------------
INPUT      <- "serie_estatal_qro_1981-2024.csv"
ANOS_CLAVE <- c(1981, 1993, 2014, 2024)

# --- 1. DATOS: superficie estatal en años clave -----------------------------
datos <- read_csv(INPUT, col_types = cols()) %>%
  filter(nivel == "total", year %in% ANOS_CLAVE) %>%
  select(year, sup_sembrada) %>%
  arrange(year) %>%
  mutate(
    base_1981  = sup_sembrada[year == 1981],
    cambio_pct = (sup_sembrada / base_1981 - 1) * 100,
    etiqueta   = if_else(year == 1981, "referencia",
                         paste0(round(cambio_pct), "%")),
    panel      = paste0(year, "\n",
                        comma(round(sup_sembrada)), " ha\n", etiqueta),
    panel      = factor(panel, levels = panel[order(year)])
  )

cat("=== DATOS PARA LOS MAPAS ===\n")
print(datos)

# --- 2. GEOMETRÍA: silueta del estado de Querétaro (entidad 22) --------------
# Se toman los municipios de mxmaps y se disuelven en un solo polígono estatal.
data("mxmunicipio.map")  # data frame de polígonos municipales (mxmaps)

qro_municipios <- mxmunicipio.map %>%
  filter(str_sub(region, 1, 2) == "22")

# Convertir a sf y disolver en silueta estatal única
qro_sf <- qro_municipios %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  group_by(group) %>%
  summarise(do_union = FALSE, .groups = "drop") %>%
  st_cast("POLYGON") %>%
  st_union() %>%
  st_sf()

# --- 3. REPLICAR LA SILUETA PARA CADA AÑO -----------------------------------
# Se crea una copia de la geometría estatal por cada año clave, asignándole
# su valor de superficie sembrada (para el relleno) y su etiqueta (para faceta).
mapas_df <- datos %>%
  rowwise() %>%
  mutate(geometry = list(qro_sf$geometry)) %>%
  ungroup() %>%
  unnest(geometry) %>%
  st_as_sf()

# --- 4. FIGURA: cuatro siluetas en fila (apaisada) --------------------------
fig <- ggplot(mapas_df) +
  geom_sf(aes(fill = sup_sembrada), color = "gray30", linewidth = 0.3) +
  facet_wrap(~ panel, nrow = 1) +
  scale_fill_gradient(
    low = "#F2E2CE", high = "#8A4B1F",
    labels = comma_format(), name = "Superficie\nsembrada (ha)"
  ) +
  labs(
    title = "Superficie sembrada en Querétaro en años clave",
    subtitle = "Color por superficie estatal; cambio porcentual respecto a 1981"
  ) +
  theme_void(base_size = 13) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 12, lineheight = 1.1),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 8))
  )

# Imagen apaisada: ancha y de poca altura
ggsave("fig_mapas_estado_anios_clave.png", fig,
       width = 14, height = 4.5, dpi = 300)
cat("\nFigura guardada: fig_mapas_estado_anios_clave.png\n")

# --- REPRODUCIBILIDAD -------------------------------------------------------
cat("\n=== sessionInfo() ===\n")
print(sessionInfo())
