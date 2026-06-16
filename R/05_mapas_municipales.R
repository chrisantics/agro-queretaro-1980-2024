# ============================================================================
# 05_mapas_municipales.R
# Proyecto: Trayectoria de la superficie sembrada en Querétaro
# ----------------------------------------------------------------------------
# OBJETIVO: Dos mapas coropléticos municipales de Querétaro (2003 y 2024),
#           coloreados por la superficie sembrada absoluta (ha) de cada
#           municipio, con etiqueta numérica de superficie. Escala de color
#           compartida entre ambos mapas para permitir comparación directa.
#
# NOTA DE ALCANCE: los datos municipales solo están disponibles 2003-2024,
#   por lo que se usan esos dos años (inicio y fin de la serie municipal).
#
# FUENTE DE DATOS:
#   - Cifras: SIACON-NG (SIAP-SADER), descargas municipales por modalidad.
#   - Geometrías: paquete R 'mxmaps' (claves INEGI).
#
# INSTALACIÓN mxmaps (si devtools falla por rlang, usar remotes):
#   install.packages("remotes"); remotes::install_github("diegovalle/mxmaps")
#
# DECISIONES METODOLÓGICAS:
#   (D6)  Se excluyen filas de subtotal "TOTAL".
#   (D10) Los 18 municipios se vinculan a sus claves INEGI (entidad 22).
#   (D15) Escala de color continua compartida entre 2003 y 2024.
#
# OUTPUT: fig_mapas_municipales_2003_2024.png
#
# Licencia código: MIT.
# ============================================================================

library(tidyverse)
library(mxmaps)
library(sf)
library(scales)
library(patchwork)
library(ggrepel)

# --- DIRECTORIO DE TRABAJO ---------------------------------------------------
# Ejecutar desde la carpeta del proyecto. En RStudio: Session >
# Set Working Directory > To Source File Location.

# --- PARÁMETROS -------------------------------------------------------------
ARCHIVO_RIEGO    <- "raw_qro_municipal_riego.txt"
ARCHIVO_TEMPORAL <- "raw_qro_municipal_temporal.txt"
ANIOS            <- c(2003, 2024)

# --- CATÁLOGO: municipio -> clave INEGI (entidad 22) (D10) -------------------
claves_qro <- tribble(
  ~municipio,             ~region,
  "Amealco de Bonfil",    "22001",
  "Pinal de Amoles",      "22002",
  "Arroyo Seco",          "22003",
  "Cadereyta de Montes",  "22004",
  "Colón",                "22005",
  "Corregidora",          "22006",
  "Ezequiel Montes",      "22007",
  "Huimilpan",            "22008",
  "Jalpan de Serra",      "22009",
  "Landa de Matamoros",   "22010",
  "El Marqués",           "22011",
  "Pedro Escobedo",       "22012",
  "Peñamiller",           "22013",
  "Querétaro",            "22014",
  "San Joaquín",          "22015",
  "San Juan del Río",     "22016",
  "Tequisquiapan",        "22017",
  "Tolimán",              "22018"
)
MUNICIPIOS_QRO <- claves_qro$municipio

# --- FUNCIÓN: PARSEO DEL TEXTO JERÁRQUICO SIACON ----------------------------
parse_siacon_mun <- function(ruta) {
  lineas <- read_lines(ruta, locale = locale(encoding = "Windows-1252"))
  registros <- list(); cy <- NA_integer_; cm <- NA_character_
  num <- function(x) suppressWarnings(as.numeric(str_replace_all(x, ",", "")))
  for (linea in lineas) {
    partes <- str_split(linea, "\t")[[1]]
    val <- str_trim(partes[1])
    if (val == "") next
    if (str_detect(val, "^[0-9]{4}$")) { cy <- as.integer(val); cm <- NA_character_; next }
    tiene_datos <- any(str_trim(partes[-1]) != "", na.rm = TRUE)
    if (!tiene_datos) {
      if (val %in% MUNICIPIOS_QRO) cm <- val
    } else {
      if (!is.na(cy) && !is.na(cm) && val != "TOTAL") {
        registros[[length(registros) + 1]] <- tibble(
          year = cy, municipio = cm, cultivo = val,
          sup_sembrada = num(partes[2])
        )
      }
    }
  }
  bind_rows(registros)
}

# --- CARGA Y AGREGACIÓN MUNICIPAL -------------------------------------------
municipal <- bind_rows(
  parse_siacon_mun(ARCHIVO_RIEGO),
  parse_siacon_mun(ARCHIVO_TEMPORAL)
)

# Serie municipal completa (todos los años disponibles), para la gráfica de líneas
serie_mun <- municipal %>%
  group_by(year, municipio) %>%
  summarise(sup_sembrada = sum(sup_sembrada, na.rm = TRUE), .groups = "drop") %>%
  left_join(claves_qro, by = "municipio")

# --- CHEQUEO: años faltantes por municipio ----------------------------------
anios_disponibles <- sort(unique(serie_mun$year))
cat("=== CHEQUEO DE COMPLETITUD ===\n")
cat("Años disponibles:", min(anios_disponibles), "-", max(anios_disponibles),
    "(", length(anios_disponibles), "años )\n")

faltantes <- serie_mun %>%
  complete(municipio, year = anios_disponibles) %>%   # expande la rejilla completa
  filter(is.na(sup_sembrada)) %>%
  group_by(municipio) %>%
  summarise(anios_faltantes = paste(year, collapse = ", "), .groups = "drop")

if (nrow(faltantes) == 0) {
  cat("Todos los municipios tienen datos en todos los años. Sin huecos.\n\n")
} else {
  cat("ADVERTENCIA: hay municipios con años faltantes:\n")
  print(faltantes, n = Inf)
  cat("\n")
}

# Subconjunto para los mapas (solo 2003 y 2024)
sup_mun <- serie_mun %>% filter(year %in% ANIOS)

cat("=== SUPERFICIE SEMBRADA POR MUNICIPIO (ha) ===\n")
sup_mun %>%
  pivot_wider(names_from = year, values_from = sup_sembrada, names_prefix = "y") %>%
  arrange(desc(y2024)) %>% print(n = Inf)

# --- CALCULAR % DE CAMBIO 2003 -> 2024 POR MUNICIPIO ------------------------
cambio_mun <- sup_mun %>%
  pivot_wider(names_from = year, values_from = sup_sembrada, names_prefix = "y") %>%
  mutate(cambio_pct = (y2024 / y2003 - 1) * 100)

# --- GEOMETRÍA MUNICIPAL DE QUERÉTARO (sf) ----------------------------------
data("mxmunicipio.map")
qro_geo <- mxmunicipio.map %>%
  filter(str_sub(region, 1, 2) == "22") %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  group_by(region) %>%
  summarise(do_union = FALSE, .groups = "drop") %>%
  st_cast("POLYGON") %>%
  st_convex_hull()   # nota: para polígonos exactos usar el shapefile original

# Centroides para etiquetas (nombre + valor)
cent_xy <- as_tibble(st_coordinates(st_centroid(qro_geo$geometry)))
centroides <- qro_geo %>%
  st_drop_geometry() %>%
  bind_cols(cent_xy) %>%
  left_join(claves_qro, by = "region")

# --- PANEL DE SUPERFICIE (nombre + valor); escala compartida (D15) ----------
lim_max <- max(sup_mun$sup_sembrada, na.rm = TRUE)

mapa_superficie <- function(anio) {
  d <- qro_geo %>% left_join(filter(sup_mun, year == anio), by = "region")
  etiq <- centroides %>% left_join(filter(sup_mun, year == anio),
                                   by = c("region", "municipio"))
  ggplot(d) +
    geom_sf(aes(fill = sup_sembrada), color = "white", linewidth = 0.3) +
    # Nombre del municipio (arriba del centroide), +10%
    geom_text(data = etiq, aes(X, Y, label = municipio),
              size = 2.3, color = "black", vjust = -0.3) +
    # Superficie en miles de ha, un decimal (abajo del centroide), +25%
    geom_text(data = etiq,
              aes(X, Y, label = sprintf("%.1f", sup_sembrada / 1e3)),
              size = 2.6, color = "black", fontface = "bold", vjust = 1.2) +
    scale_fill_gradient(low = "#F2E2CE", high = "#8A4B1F",
                        limits = c(0, lim_max),
                        labels = function(x) sprintf("%.0f", x / 1e3),
                        name = "Superficie\nsembrada\n(miles de ha)") +
    labs(title = as.character(anio)) +
    theme_void(base_size = 13) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
}

# --- PANEL DE % DE CAMBIO (divergente, nombre + valor) ----------------------
lim_pct <- max(abs(cambio_mun$cambio_pct), na.rm = TRUE)

mapa_cambio <- function() {
  d <- qro_geo %>% left_join(cambio_mun, by = "region")
  etiq <- centroides %>% left_join(cambio_mun, by = c("region", "municipio"))
  ggplot(d) +
    geom_sf(aes(fill = cambio_pct), color = "white", linewidth = 0.3) +
    # Nombre del municipio (arriba), +10%
    geom_text(data = etiq, aes(X, Y, label = municipio),
              size = 2.3, color = "black", vjust = -0.3) +
    # Porcentaje de cambio (abajo), +30%
    geom_text(data = etiq,
              aes(X, Y, label = sprintf("%+.0f%%", cambio_pct)),
              size = 2.7, color = "black", fontface = "bold", vjust = 1.2) +
    scale_fill_gradient2(low = "#A32D2D", mid = "#F5F5F0", high = "#0F6E56",
                         midpoint = 0, limits = c(-lim_pct, lim_pct),
                         labels = label_number(suffix = "%"),
                         name = "Cambio\n2003–2024") +
    labs(title = "Cambio 2003–2024") +
    theme_void(base_size = 13) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
}

# --- FIGURA: tres paneles en fila (2003 | 2024 | % cambio) ------------------
fig <- mapa_superficie(2003) + mapa_superficie(2024) + mapa_cambio() +
  plot_layout(nrow = 1) +
  plot_annotation(
    title = "Superficie sembrada por municipio en Querétaro",
    subtitle = "Superficie absoluta (2003 y 2024) y cambio porcentual"
  )

ggsave("fig_mapas_municipales_2003_2024.png", fig,
       width = 18, height = 6.5, dpi = 300)
cat("\nFigura (mapas) guardada: fig_mapas_municipales_2003_2024.png\n")

# --- GRÁFICA DE LÍNEAS: serie por municipio 2003-2024 -----------------------
# Una línea por municipio. Se etiqueta el extremo derecho con ggrepel para
# poder identificar cada serie sin saturar de leyenda.
etiqueta_fin <- serie_mun %>%
  filter(year == max(anios_disponibles))

fig_lineas <- ggplot(serie_mun,
                     aes(year, sup_sembrada / 1e3, color = municipio)) +
  geom_line(linewidth = 0.7) +
  geom_text_repel(data = etiqueta_fin,
                  aes(label = municipio),
                  size = 3, hjust = 0, direction = "y",
                  xlim = c(max(anios_disponibles) + 0.5, NA),
                  segment.size = 0.2, segment.color = "gray70",
                  max.overlaps = Inf, show.legend = FALSE) +
  scale_x_continuous(breaks = c(seq(2003, 2021, 3), 2024),
                     limits = c(2003, max(anios_disponibles) + 6)) +
  scale_y_continuous(labels = comma_format(accuracy = 1)) +
  labs(x = "Año", y = "Superficie sembrada (miles de ha)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",          # etiquetas al final sustituyen leyenda
        panel.grid.minor = element_blank())

ggsave("fig_lineas_municipales_2003_2024.png", fig_lineas,
       width = 11, height = 7, dpi = 300)
cat("Figura (líneas) guardada: fig_lineas_municipales_2003_2024.png\n")

# --- REPRODUCIBILIDAD -------------------------------------------------------
cat("\n=== sessionInfo() ===\n")
print(sessionInfo())
