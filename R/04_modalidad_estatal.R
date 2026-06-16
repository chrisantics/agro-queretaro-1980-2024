# ============================================================================
# 04_modalidad_estatal.R
# Proyecto: Trayectoria de la superficie sembrada en Querétaro (1981-2024)
# ----------------------------------------------------------------------------
# OBJETIVO: Serie estatal de superficie sembrada por modalidad (riego y
#           temporal), 1981-2024, y figura de dos curvas absolutas.
#           Imprime: ha en 1981 y 2024 por modalidad, cambio porcentual por
#           modalidad, y participación de cada modalidad en el total estatal.
#
# FUENTE DE DATOS:
#   SIACON-NG (SIAP-SADER), consultas ESTATALES por modalidad para Querétaro,
#   1980-2024. Archivos:
#     raw_qro_estatal_riego.txt
#     raw_qro_estatal_temporal.txt
#   Estructura jerárquica delimitada por tabuladores (Año > Modalidad >
#   Cultivo > datos), codificación Windows-1252.
#
# VALIDACIÓN:
#   La suma (riego + temporal) por año coincide con la serie estatal total
#   (p. ej. 1981 = 227,007 ha). Diferencia < 0.4% en 2024 (redondeos).
#
# DECISIONES METODOLÓGICAS:
#   (D1) Se excluye 1980 (consistente con el resto del análisis).
#   (D6) Se excluyen filas de subtotal "TOTAL".
#
# OUTPUT:
#   serie_modalidad_estatal_1981-2024.csv
#   fig_modalidad_estatal.png
#
# Licencia código: MIT.
# ============================================================================

library(tidyverse)
library(scales)

# --- DIRECTORIO DE TRABAJO ---------------------------------------------------
# Este script asume que se ejecuta desde la carpeta del proyecto. En RStudio,
# abrir el proyecto (.Rproj) o usar Session > Set Working Directory >
# To Source File Location.

# --- PARÁMETROS -------------------------------------------------------------
ARCHIVO_RIEGO    <- "raw_qro_estatal_riego.txt"
ARCHIVO_TEMPORAL <- "raw_qro_estatal_temporal.txt"
ANO_EXCLUIR      <- 1980          # (D1)
OUTPUT_CSV       <- "serie_modalidad_estatal_1981-2024.csv"

# --- FUNCIÓN: PARSEO DEL TEXTO JERÁRQUICO SIACON (estatal) ------------------
# Jerarquía: Año > Modalidad ("Riego"/"Temporal") > Cultivo (fila-dato).
parse_siacon_estatal <- function(ruta) {
  lineas <- read_lines(ruta, locale = locale(encoding = "Windows-1252"))
  registros <- list(); cy <- NA_integer_
  num <- function(x) suppressWarnings(as.numeric(str_replace_all(x, ",", "")))
  for (linea in lineas) {
    partes <- str_split(linea, "\t")[[1]]
    val <- str_trim(partes[1])
    if (val == "") next
    if (str_detect(val, "^[0-9]{4}$")) { cy <- as.integer(val); next }
    if (val %in% c("Riego", "Temporal")) next   # encabezado de modalidad
    tiene_datos <- any(str_trim(partes[-1]) != "", na.rm = TRUE)
    if (tiene_datos && !is.na(cy) && val != "TOTAL") {
      registros[[length(registros) + 1]] <- tibble(
        year = cy, cultivo = val, sup_sembrada = num(partes[2])
      )
    }
  }
  bind_rows(registros)
}

# --- CARGA Y AGREGACIÓN -----------------------------------------------------
riego <- parse_siacon_estatal(ARCHIVO_RIEGO) %>%
  mutate(modalidad = "Riego")
temporal <- parse_siacon_estatal(ARCHIVO_TEMPORAL) %>%
  mutate(modalidad = "Temporal")

serie_modalidad <- bind_rows(riego, temporal) %>%
  filter(year != ANO_EXCLUIR) %>%                 # (D1)
  group_by(year, modalidad) %>%
  summarise(sup_sembrada = sum(sup_sembrada, na.rm = TRUE), .groups = "drop")

write_csv(serie_modalidad, OUTPUT_CSV)

# --- ESTADÍSTICAS SOLICITADAS -----------------------------------------------
resumen <- serie_modalidad %>%
  filter(year %in% c(1981, 2024)) %>%
  pivot_wider(names_from = year, values_from = sup_sembrada,
              names_prefix = "y")

totales <- serie_modalidad %>%
  filter(year %in% c(1981, 2024)) %>%
  group_by(year) %>%
  summarise(total = sum(sup_sembrada), .groups = "drop")

tot_1981 <- totales$total[totales$year == 1981]
tot_2024 <- totales$total[totales$year == 2024]

resumen <- resumen %>%
  mutate(
    cambio_pct   = (y2024 / y1981 - 1) * 100,
    part_1981_pct = y1981 / tot_1981 * 100,
    part_2024_pct = y2024 / tot_2024 * 100
  )

cat("=== SUPERFICIE POR MODALIDAD (ha) Y CAMBIO 1981 -> 2024 ===\n")
resumen %>%
  transmute(
    Modalidad = modalidad,
    `1981 (ha)` = round(y1981),
    `2024 (ha)` = round(y2024),
    `Cambio %`  = round(cambio_pct),
    `% del total 1981` = round(part_1981_pct),
    `% del total 2024` = round(part_2024_pct)
  ) %>%
  print()

# --- FIGURA: dos curvas absolutas (sin título interno) ----------------------
fig <- ggplot(serie_modalidad,
              aes(x = year, y = sup_sembrada / 1e3, color = modalidad)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("Riego" = "#185FA5", "Temporal" = "#D85A30"),
                     name = NULL) +
  scale_x_continuous(breaks = c(seq(1981, 2021, 4), 2024)) +   # 2024 explícito
  scale_y_continuous(labels = comma_format(accuracy = 1)) +
  labs(x = "Año", y = "Superficie sembrada (miles de ha)") +
  theme_minimal(base_size = 15) +   # +15% respecto a 13
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = rel(1.15)),   # títulos de ejes +15%
        axis.text  = element_text(size = rel(1.15)),   # valores de ejes +15%
        legend.text = element_text(size = rel(1.15)))  # leyenda de series +15%

ggsave("fig_modalidad_estatal.png", fig,
       width = 10, height = 5.8, dpi = 300)
cat("\nFigura guardada: fig_modalidad_estatal.png\n")

# --- REPRODUCIBILIDAD -------------------------------------------------------
cat("\n=== sessionInfo() ===\n")
print(sessionInfo())
