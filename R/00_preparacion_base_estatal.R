# ============================================================================
# 00_preparacion_base_estatal.R
# Proyecto: Trayectoria de la superficie sembrada en Querétaro (1981-2024)
# ----------------------------------------------------------------------------
# OBJETIVO: A partir de la consulta cruda de SIACON a nivel estatal, generar la
#           base agregada y limpia (data_agregada_base.csv) que sirve como
#           punto de partida del análisis. Realiza la estandarización de
#           columnas, la exclusión de registros agregados y la agregación a
#           nivel año-estado-región-grupo de cultivo.
#
# NOTA DE ALCANCE:
#   Este script corresponde a un procesamiento previo, independiente del
#   análisis específico de Querétaro. La consulta cruda estatal de origen
#   (raw_Agro_estatal_1980-2024.csv) se distribuye en el dataset de Kaggle, no en GitHub; este script se
#   incluye para documentar de forma transparente cómo se obtuvo el archivo
#   data_agregada_base.csv, que es el punto de partida declarado del análisis.
#   (Versión que conserva únicamente la etapa de limpieza; el análisis nacional
#   de quiebres del script original pertenece a otro estudio y se omite aquí.)
#
# FUENTE DE DATOS:
#   Sistema de Información Agroalimentaria de Consulta (SIACON-NG, SIAP-SADER),
#   consulta estatal con desglose por grupo de cultivo, 1980-2024.
#
# INPUT:  raw_Agro_estatal_1980-2024.csv   (consulta cruda estatal; disponible en Kaggle)
# OUTPUT: data_agregada_base.csv
#
# DECISIONES METODOLÓGICAS:
#   (D1) Se excluye 1980: dato atípicamente bajo y sin punto de comparación
#        con 1979 para evaluar la tendencia.
#   (D2) Se estandarizan los nombres de columnas (alfanuméricos, minúsculas).
#   (D3) Se eliminan filas con NA en variables clave (típicamente totales
#        agregados que no corresponden a registros de cultivo).
#   (D4) Se excluyen los registros agregados "Nacional" tanto en estado como en
#        grupo_natural, para evitar dobles conteos.
#
# Licencia código: MIT.
# ============================================================================

library(tidyverse)

# --- DIRECTORIO DE TRABAJO ---------------------------------------------------
# Este script asume que se ejecuta desde la carpeta del proyecto, donde se
# encuentra el archivo de datos de entrada. En RStudio, abrir el proyecto
# (.Rproj) o usar Session > Set Working Directory > To Source File Location.

# --- PARÁMETROS -------------------------------------------------------------
INPUT             <- "raw_Agro_estatal_1980-2024.csv"   # consulta cruda estatal (en Kaggle)
OUTPUT            <- "raw_data_agregada_base.csv"
ANOS_EXCLUIR      <- c(1980)                 # (D1)
EXCLUIR_AGREGADOS <- c("Nacional", "NACIONAL")  # (D4)

# --- CARGA Y LIMPIEZA DE NOMBRES (D2) ---------------------------------------
data_raw <- read_csv(
  INPUT,
  col_types = cols(.default = "n",
                   grupo_natural = "c", region = "c", estado = "c")
)

data_clean <- data_raw %>%
  rename_with(~ str_replace_all(., "[^[:alnum:]]", "_")) %>%
  rename_with(~ tolower(.), everything()) %>%
  rename(
    sup_sembrada       = superficie_sembrada_ha,
    sup_cosechada      = superficie_cosechada_ha,
    sup_siniestrada    = superficie_siniestrada_ha,
    produccion         = produccion_ton,
    rendimiento        = rendimiento_obtenido_ton_ha,
    precio_medio_rural = precio_medio_rural___ton,
    valor_nominal      = valor_de_la_produccion_miles_de_pesos
  ) %>%
  filter(!(year %in% ANOS_EXCLUIR)) %>%                 # (D1)
  drop_na(sup_cosechada, produccion, valor_nominal)     # (D3)

cat("=== VERIFICACIÓN INICIAL ===\n")
cat("Años disponibles tras filtrar:", toString(sort(unique(data_clean$year))), "\n")

# --- AGREGACIÓN A NIVEL AÑO-ESTADO-REGIÓN-GRUPO (D4) ------------------------
data_agregada_base <- data_clean %>%
  filter(!(estado %in% EXCLUIR_AGREGADOS)) %>%          # (D4) total estatal
  filter(!(grupo_natural %in% EXCLUIR_AGREGADOS)) %>%   # (D4) subtotal de grupo
  group_by(year, estado, region, grupo_natural) %>%
  summarise(
    sup_sembrada = sum(sup_sembrada, na.rm = TRUE),
    produccion   = sum(produccion,   na.rm = TRUE),
    .groups = "drop"
  )

# --- VERIFICACIÓN DE COBERTURA (SIACON vs INEGI 2022) -----------------------
nacional_2022 <- data_agregada_base %>%
  filter(year == 2022) %>%
  summarise(sup = sum(sup_sembrada, na.rm = TRUE)) %>%
  pull(sup)

cat("\n=== VERIFICACIÓN DE COBERTURA ===\n")
cat("Superficie sembrada nacional 2022 (recalculada):",
    format(round(nacional_2022), big.mark = ","), "ha\n")
cat("Referencia INEGI 2022: 21,635,876 ha (cobertura cercana al 95%).\n")

# --- GUARDAR ----------------------------------------------------------------
write_csv(data_agregada_base, OUTPUT)
cat("\nArchivo guardado:", OUTPUT, "\n")

# --- REPRODUCIBILIDAD -------------------------------------------------------
cat("\n=== sessionInfo() ===\n")
print(sessionInfo())
