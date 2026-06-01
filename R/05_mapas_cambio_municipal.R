# ============================================================================
# 05_mapas_cambio_municipal.R
# Proyecto: Capacidad productiva agrícola de Querétaro
# ----------------------------------------------------------------------------
# OBJETIVO: Mapas coropléticos del CAMBIO ABSOLUTO (hectáreas) de la superficie
#           sembrada por municipio entre 2003 y 2024, separando riego y temporal.
#           Identifica qué municipios perdieron (o ganaron) más SUPERFICIE REAL.
#
# JUSTIFICACIÓN METODOLÓGICA (D11):
#   Se mapea el cambio ABSOLUTO en hectáreas (sup_2024 - sup_2003), NO el cambio
#   porcentual. El cambio porcentual distorsiona en municipios de base pequeña
#   (p. ej., Pinal de Amoles pasa de 13 a 33 ha de riego = +154 %, lo que
#   visualmente exagera un cambio de apenas 20 ha, frente a Querétaro que pierde
#   514 ha reales pero solo -18 %). Para la pregunta "qué municipios perdieron
#   más superficie productiva", la métrica correcta es la hectárea, no el %.
#
# FUENTE DE DATOS:
#   - Cifras: SIACON-NG (SIAP-SADER), descargas municipales por modalidad.
#   - Geometrías: paquete R 'mxmaps' (municipios de México, claves INEGI).
#
# INSTALACIÓN DE mxmaps (si devtools falla por conflicto de rlang, usar remotes):
#   install.packages("remotes"); remotes::install_github("diegovalle/mxmaps")
#
# DECISIONES METODOLÓGICAS:
#   (D6)  Se excluyen filas de subtotal "TOTAL".
#   (D10) Los 18 municipios se vinculan a sus claves INEGI (entidad 22).
#   (D11) Cambio absoluto en ha; escala de color divergente centrada en 0.
#
# OUTPUT:
#   fig_mapas_cambio_modalidad.png  (dos mapas: riego y temporal, cambio en ha)
#   tabla_cambio_municipal.csv
#
# Licencia código: MIT.
# ============================================================================

library(tidyverse)
library(mxmaps)
library(scales)
library(patchwork)

# --- DIRECTORIO DE TRABAJO ---------------------------------------------------
# Este script asume que se ejecuta desde la carpeta del proyecto, donde se
# encuentran los archivos de datos de entrada. En RStudio, abrir el proyecto
# (.Rproj) o usar Session > Set Working Directory > To Source File Location.

ARCHIVO_RIEGO    <- "dato_agricola_qro_municipal_riego.txt"
ARCHIVO_TEMPORAL <- "dato_agricola_qro_municipal_temporal.txt"
ANIO_INI <- 2003
ANIO_FIN <- 2024

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

# --- CARGA Y CAMBIO ABSOLUTO (D11) ------------------------------------------
riego    <- parse_siacon_mun(ARCHIVO_RIEGO)    %>% mutate(modalidad = "Riego")
temporal <- parse_siacon_mun(ARCHIVO_TEMPORAL) %>% mutate(modalidad = "Temporal")

cambio_mun <- bind_rows(riego, temporal) %>%
  filter(year %in% c(ANIO_INI, ANIO_FIN)) %>%
  group_by(year, municipio, modalidad) %>%
  summarise(sup_sembrada = sum(sup_sembrada, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = sup_sembrada,
              names_prefix = "y") %>%
  mutate(cambio_ha = .data[[paste0("y", ANIO_FIN)]] -
                     .data[[paste0("y", ANIO_INI)]]) %>%
  left_join(claves_qro, by = "municipio")

write_csv(cambio_mun, "tabla_cambio_municipal.csv")

cat("=== CAMBIO ABSOLUTO (ha) DE SUPERFICIE SEMBRADA POR MUNICIPIO (",
    ANIO_INI, "->", ANIO_FIN, ") ===\n")
cambio_mun %>%
  select(municipio, modalidad, cambio_ha) %>%
  arrange(modalidad, cambio_ha) %>%
  print(n = Inf)

# --- LÍMITE SIMÉTRICO PARA LA ESCALA DIVERGENTE -----------------------------
# Compartido entre ambos mapas para que los colores sean comparables.
lim <- max(abs(cambio_mun$cambio_ha), na.rm = TRUE)

# --- FUNCIÓN: MAPA DE CAMBIO EN HA DE UNA MODALIDAD -------------------------
mapa_cambio <- function(datos, modalidad_sel, titulo) {
  df <- datos %>%
    filter(modalidad == modalidad_sel) %>%
    transmute(region, value = cambio_ha)

  mxmunicipio_choropleth(
    df,
    num_colors = 1,
    zoom = subset(df_mxmunicipio, state_code == "22")$region,
    title = titulo
  ) +
    scale_fill_gradient2(
      low = "#A32D2D", mid = "#F5F5F0", high = "#0F6E56",
      midpoint = 0, limits = c(-lim, lim),
      labels = comma_format(),
      name = "Cambio (ha)"
    )
}

# --- FIGURA: dos mapas (riego y temporal) -----------------------------------
mapa_riego    <- mapa_cambio(cambio_mun, "Riego",    "Riego")
mapa_temporal <- mapa_cambio(cambio_mun, "Temporal", "Temporal")

fig_mapas <- mapa_riego + mapa_temporal +
  plot_annotation(
    title = paste0("Cambio en la superficie sembrada por municipio, ",
                   ANIO_INI, "-", ANIO_FIN, " (ha)"),
    subtitle = "Rojo = pérdida; verde = aumento. Escala compartida entre paneles."
  )

ggsave("fig_mapas_cambio_modalidad.png", fig_mapas,
       width = 12, height = 6, dpi = 300)
cat("\nFigura guardada: fig_mapas_cambio_modalidad.png\n")

# --- REPRODUCIBILIDAD -------------------------------------------------------
cat("\n=== sessionInfo() ===\n")
print(sessionInfo())
