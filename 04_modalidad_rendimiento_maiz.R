# ============================================================================
# 03b_rendimiento_maiz.R
# Proyecto: Capacidad productiva agrícola de Querétaro
# ----------------------------------------------------------------------------
# OBJETIVO: Calcular el rendimiento REAL del maíz (grano y forrajero) por
#           modalidad (riego y temporal) en Querétaro, y producir la Figura 3
#           del artículo NTHE (reemplaza al "desacople" de rendimiento implícito).
#
# JUSTIFICACIÓN METODOLÓGICA:
#   El rendimiento implícito agregado (producción total / superficie total)
#   mezcla cultivos con escalas ton/ha no comparables y confunde la mejora
#   productiva real con el cambio en la composición del mosaico de cultivos.
#   Para medir intensificación de forma rigurosa se usa el rendimiento de un
#   cultivo comparable consigo mismo a lo largo del tiempo: el maíz.
#   Se separa maíz GRANO (alimentario) de maíz FORRAJERO (ganadero), pues
#   tienen escalas de rendimiento radicalmente distintas (~1-9 vs ~60 ton/ha).
#
# FUENTE DE DATOS:
#   SIACON-NG (SIAP/SADER), descargas municipales por modalidad para Querétaro.
#   Periodo disponible: 2003-2024.
#   Archivos:
#     dato_agricola_qro_municipal_riego.txt
#     dato_agricola_qro_municipal_temporal.txt
#   Estructura jerárquica delimitada por tabuladores, codificación cp1252.
#
# DECISIONES METODOLÓGICAS:
#   (D8) Rendimiento agregado = sum(producción) / sum(superficie cosechada)
#        sobre los 18 municipios. NO se promedia la columna de rendimiento
#        (eso daría un promedio no ponderado e incorrecto). El rendimiento
#        SIACON se define sobre superficie COSECHADA, no sembrada.
#   (D9) "Maíz grano" agrupa las variedades blanco, amarillo, de color y
#        s/clasificar. "Maíz forrajero" toma las variedades en verde.
#   (D6) Se excluyen filas de subtotal con etiqueta "TOTAL".
#
# OUTPUTS:
#   serie_rend_maiz_qro_2003-2024.csv
#   fig3_rendimiento_maiz.png   (dos paneles: grano y forrajero; escala libre)
#
# NOTA DE ALCANCE: cubre 2003-2024 (las Figuras 1-2 abarcan 1981-2024).
#
# Licencia código: MIT.
# ============================================================================

library(tidyverse)
library(scales)

# --- DIRECTORIO DE TRABAJO ---------------------------------------------------
# Este script asume que se ejecuta desde la carpeta del proyecto, donde se
# encuentran los archivos de datos de entrada. En RStudio, abrir el proyecto
# (.Rproj) o usar Session > Set Working Directory > To Source File Location.

ARCHIVO_RIEGO    <- "dato_agricola_qro_municipal_riego.txt"
ARCHIVO_TEMPORAL <- "dato_agricola_qro_municipal_temporal.txt"
OUTPUT_CSV       <- "serie_rend_maiz_qro_2003-2024.csv"

MUNICIPIOS_QRO <- c(
  "Amealco de Bonfil","Arroyo Seco","Cadereyta de Montes","Colón","Corregidora",
  "Ezequiel Montes","El Marqués","Huimilpan","Jalpan de Serra","Landa de Matamoros",
  "Pedro Escobedo","Peñamiller","Querétaro","San Joaquín","San Juan del Río",
  "Tequisquiapan","Tolimán","Coroneo","Pinal de Amoles"
)

# --- FUNCIÓN: PARSEO DEL TEXTO JERÁRQUICO SIACON ----------------------------
# Captura, para cada fila-dato, la superficie cosechada y la producción,
# heredando año y municipio de las filas-encabezado.
parse_siacon_maiz <- function(ruta) {
  lineas <- read_lines(ruta, locale = locale(encoding = "Windows-1252"))
  registros <- list()
  cy <- NA_integer_; cm <- NA_character_
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
          year      = cy,
          municipio = cm,
          cultivo   = val,
          cosechada = num(partes[3]),   # columna 3 = superficie cosechada
          produccion = num(partes[5])   # columna 5 = producción (ton)
        )
      }
    }
  }
  bind_rows(registros)
}

# --- CARGA ------------------------------------------------------------------
riego    <- parse_siacon_maiz(ARCHIVO_RIEGO)    %>% mutate(modalidad = "Riego")
temporal <- parse_siacon_maiz(ARCHIVO_TEMPORAL) %>% mutate(modalidad = "Temporal")
maiz <- bind_rows(riego, temporal)

# --- CLASIFICACIÓN DE MAÍZ (D9) ---------------------------------------------
maiz_clasificado <- maiz %>%
  filter(str_starts(cultivo, "Maíz")) %>%
  mutate(tipo_maiz = case_when(
    str_starts(cultivo, "Maíz grano")     ~ "Maíz grano",
    str_starts(cultivo, "Maíz forrajero") ~ "Maíz forrajero",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(tipo_maiz))

# --- RENDIMIENTO AGREGADO (D8) ----------------------------------------------
serie_rend <- maiz_clasificado %>%
  group_by(year, tipo_maiz, modalidad) %>%
  summarise(
    produccion = sum(produccion, na.rm = TRUE),
    cosechada  = sum(cosechada,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(rendimiento = produccion / cosechada)

write_csv(serie_rend, OUTPUT_CSV)
cat("Archivo guardado:", OUTPUT_CSV, "\n\n")

# --- VERIFICACIÓN -----------------------------------------------------------
cat("=== RENDIMIENTO DEL MAÍZ (ton/ha) — años extremos ===\n")
serie_rend %>%
  filter(year %in% c(2003, 2024)) %>%
  select(year, tipo_maiz, modalidad, rendimiento) %>%
  arrange(tipo_maiz, modalidad, year) %>%
  print(n = Inf)

# --- FIGURA 3: rendimiento del maíz, dos paneles (escala libre) -------------
fig3 <- ggplot(serie_rend,
               aes(year, rendimiento, color = modalidad)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  facet_wrap(~ tipo_maiz, scales = "free_y") +
  scale_color_manual(values = c("Riego" = "#0072B2", "Temporal" = "#D55E00")) +
  scale_x_continuous(breaks = seq(2003, 2024, 3)) +
  labs(x = "Año", y = "Rendimiento (ton/ha)", color = "Modalidad") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

ggsave("fig3_rendimiento_maiz.png", fig3,
       width = 11, height = 5.5, dpi = 300)
cat("\nFigura 3 guardada: rendimiento del maíz por tipo y modalidad.\n")

# --- REPRODUCIBILIDAD -------------------------------------------------------
cat("\n=== sessionInfo() ===\n")
print(sessionInfo())
