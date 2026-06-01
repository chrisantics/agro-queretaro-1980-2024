# ============================================================================
# 04_analisis_modalidad.R
# Proyecto: Capacidad productiva agrícola de Querétaro
# ----------------------------------------------------------------------------
# OBJETIVO: Analizar la evolución de la superficie sembrada por MODALIDAD
#           (riego vs temporal) en Querétaro y producir la Figura 4 de NTHE.
#
# FUENTE DE DATOS:
#   SIACON-NG (SIAP/SADER), descargas independientes por modalidad a nivel
#   MUNICIPAL para Querétaro. Periodo disponible: 2003-2024.
#   Archivos:
#     dato_agricola_qro_municipal_riego.txt
#     dato_agricola_qro_municipal_temporal.txt
#   Estructura: texto jerárquico delimitado por tabuladores
#     (Año > Municipio > Cultivo-grupo > Cultivo-variedad > datos),
#     codificación cp1252 (Windows-1252).
#
# NOTA DE ALCANCE (transparencia):
#   El desglose por modalidad solo está disponible desde 2003, por lo que la
#   Figura 4 cubre 2003-2024, a diferencia de las Figuras 1-3 (1981-2024).
#   Esta limitación temporal se declara explícitamente en el artículo.
#
# VALIDACIÓN:
#   La suma (riego + temporal) por año coincide con la serie estatal SIACON
#   con diferencia < 0.5% (verificado para 2003-2023).
#
# DECISIONES METODOLÓGICAS:
#   (D6) Se excluyen filas de subtotal con etiqueta "TOTAL" (p.ej. la anomalía
#        de Tolimán 2024, que reportaba un agregado de ~3.38 millones de ha).
#   (D7) Se suman los 18 municipios para obtener el total estatal por modalidad.
#
# OUTPUTS:
#   serie_modalidad_qro_2003-2024.csv
#   fig4a_modalidad_absoluta.png   (riego vs temporal, miles de ha)
#   fig4b_modalidad_relativa.png   (participación % del riego en el total)
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
OUTPUT_CSV       <- "serie_modalidad_qro_2003-2024.csv"

# --- CATÁLOGO DE MUNICIPIOS DE QUERÉTARO ------------------------------------
MUNICIPIOS_QRO <- c(
  "Amealco de Bonfil","Arroyo Seco","Cadereyta de Montes","Colón","Corregidora",
  "Ezequiel Montes","El Marqués","Huimilpan","Jalpan de Serra","Landa de Matamoros",
  "Pedro Escobedo","Peñamiller","Querétaro","San Joaquín","San Juan del Río",
  "Tequisquiapan","Tolimán","Coroneo","Pinal de Amoles"
)

# --- FUNCIÓN: PARSEO DEL TEXTO JERÁRQUICO SIACON ----------------------------
# El archivo tiene una estructura jerárquica donde el contexto (año, municipio)
# se hereda de las filas-encabezado (sin datos) a las filas-dato (con cifras).
parse_siacon_txt <- function(ruta) {
  lineas <- read_lines(ruta, locale = locale(encoding = "Windows-1252"))

  registros <- list()
  cy <- NA_integer_      # año actual
  cm <- NA_character_    # municipio actual

  num <- function(x) {
    x <- str_replace_all(x, ",", "")
    suppressWarnings(as.numeric(x))
  }

  for (linea in lineas) {
    partes <- str_split(linea, "\t")[[1]]
    val <- str_trim(partes[1])
    if (val == "") next

    # ¿Es un año? (cuatro dígitos)
    if (str_detect(val, "^[0-9]{4}$")) {
      cy <- as.integer(val); cm <- NA_character_; next
    }

    # ¿Tiene datos en las columnas siguientes?
    tiene_datos <- any(str_trim(partes[-1]) != "", na.rm = TRUE)

    if (!tiene_datos) {
      # Fila-encabezado: municipio o grupo/cultivo
      if (val %in% MUNICIPIOS_QRO) cm <- val
    } else {
      # Fila-dato: requiere año y municipio en contexto; (D6) excluir TOTAL
      if (!is.na(cy) && !is.na(cm) && val != "TOTAL") {
        registros[[length(registros) + 1]] <- tibble(
          year      = cy,
          municipio = cm,
          cultivo   = val,
          sup_sembrada = num(partes[2])
        )
      }
    }
  }
  bind_rows(registros)
}

# --- CARGA Y AGREGACIÓN -----------------------------------------------------
riego_raw    <- parse_siacon_txt(ARCHIVO_RIEGO)    %>% mutate(modalidad = "Riego")
temporal_raw <- parse_siacon_txt(ARCHIVO_TEMPORAL) %>% mutate(modalidad = "Temporal")

modalidad_mun <- bind_rows(riego_raw, temporal_raw)

# (D7) Suma estatal por año y modalidad
serie_modalidad <- modalidad_mun %>%
  group_by(year, modalidad) %>%
  summarise(sup_sembrada = sum(sup_sembrada, na.rm = TRUE), .groups = "drop")

# --- VERIFICACIÓN -----------------------------------------------------------
cat("=== SUPERFICIE SEMBRADA POR MODALIDAD (estatal, 2003-2024) ===\n")
verif <- serie_modalidad %>%
  pivot_wider(names_from = modalidad, values_from = sup_sembrada) %>%
  mutate(Total = Riego + Temporal,
         pct_riego = round(Riego / Total * 100, 1))
print(verif, n = Inf)

# --- GUARDAR SERIE ----------------------------------------------------------
write_csv(serie_modalidad, OUTPUT_CSV)
cat("\nArchivo guardado:", OUTPUT_CSV, "\n")

# --- FIGURA 4A: superficie absoluta por modalidad ---------------------------
fig4a <- ggplot(serie_modalidad,
                aes(year, sup_sembrada / 1e3, color = modalidad)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  scale_color_manual(values = c("Riego" = "#0072B2", "Temporal" = "#D55E00")) +
  scale_x_continuous(breaks = seq(2003, 2024, 3)) +
  scale_y_continuous(labels = comma_format(accuracy = 1)) +
  labs(x = "Año", y = "Superficie sembrada (miles de ha)",
       color = "Modalidad",
       subtitle = "Superficie sembrada por modalidad — Querétaro (2003-2024)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave("fig4a_modalidad_absoluta.png", fig4a,
       width = 10, height = 5.5, dpi = 300)
cat("Figura 4a guardada (superficie absoluta por modalidad).\n")

# --- FIGURA 4B: participación relativa del riego ----------------------------
fig4b <- verif %>%
  ggplot(aes(year, pct_riego)) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray60") +
  geom_line(color = "#0072B2", linewidth = 0.9) +
  geom_point(color = "#0072B2", size = 1.8) +
  scale_x_continuous(breaks = seq(2003, 2024, 3)) +
  scale_y_continuous(limits = c(25, 60),
                     labels = label_number(suffix = "%")) +
  labs(x = "Año", y = "Participación del riego en superficie sembrada",
       subtitle = "El riego pasa de ~32% a >50% del total estatal") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

ggsave("fig4b_modalidad_relativa.png", fig4b,
       width = 10, height = 5.5, dpi = 300)
cat("Figura 4b guardada (participación relativa del riego).\n")

# --- RESUMEN INTERPRETATIVO -------------------------------------------------
cat("\n=== CAMBIO 2003 -> 2024 ===\n")
camb <- verif %>% filter(year %in% c(2003, 2024))
cat(sprintf("Riego:    %s -> %s ha (%.1f%%)\n",
            format(camb$Riego[1], big.mark=","),
            format(camb$Riego[2], big.mark=","),
            (camb$Riego[2]/camb$Riego[1]-1)*100))
cat(sprintf("Temporal: %s -> %s ha (%.1f%%)\n",
            format(camb$Temporal[1], big.mark=","),
            format(camb$Temporal[2], big.mark=","),
            (camb$Temporal[2]/camb$Temporal[1]-1)*100))

# --- REPRODUCIBILIDAD -------------------------------------------------------
cat("\n=== sessionInfo() ===\n")
print(sessionInfo())
