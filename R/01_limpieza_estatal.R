# ============================================================================
# 00_limpieza_estatal.R
# Proyecto: Capacidad productiva agrícola de Querétaro (1981-2024)
# ----------------------------------------------------------------------------
# OBJETIVO: A partir de la base SIACON estatal agregada, generar la serie
#           anual limpia de Querétaro lista para análisis.
#
# FUENTE DE DATOS:
#   Sistema de Información Agroalimentaria de Consulta (SIACON-NG), SIAP/SADER.
#   URL: https://www.gob.mx/siap/documentos/siacon-ng-161430
#   Descarga: 1 de diciembre 2025.
#   Parámetros de consulta: todos los estados, todas las modalidades,
#   agricultura protegida, todos los tipos de producción y mercado.
#   Periodo 1980-2024. Variables: superficie sembrada/cosechada/siniestrada,
#   producción, rendimiento, precio medio rural, valor de la producción.
#
# INPUT:  data_agregada_base.csv  (ya pre-agregada por year-estado-region-grupo)
# OUTPUT: data/processed/serie_estatal_qro_1981-2024.csv
#
# DECISIONES METODOLÓGICAS EXPLÍCITAS:
#   (D1) Se excluye 1980: dato atípicamente bajo y sin punto de comparación
#        con 1979 para evaluar tendencia previa (ver borrador NTHE).
#   (D2) Se excluyen registros agregados "Nacional" en estado y grupo_natural
#        para evitar doble conteo.
#   (D3) Validación cruzada: la suma estatal por año coincide con la base
#        municipal SIACON 2003-2023 con diferencia < 0.5%.
#
# Autor: Cedillo-Jiménez C.A. y colaboradores.
# Licencia código: MIT.
# ============================================================================

library(tidyverse)

# --- PARÁMETROS -------------------------------------------------------------
ESTADO_OBJETIVO <- "Queretaro"   # Nombre normalizado en la base (sin acento)
ANOS_EXCLUIR    <- c(1980)       # (D1)
EXCLUIR_AGREG   <- c("Nacional", "NACIONAL")  # (D2)

INPUT  <- "data_agregada_base.csv"
OUTPUT <- "serie_estatal_qro_1981-2024.csv"

# --- CARGA ------------------------------------------------------------------
data_raw <- read_csv(INPUT,
                     col_types = cols(.default = "n",
                                      estado = "c",
                                      region = "c",
                                      grupo_natural = "c"))

# --- LIMPIEZA ---------------------------------------------------------------
data_clean <- data_raw %>%
  filter(!year %in% ANOS_EXCLUIR) %>%                # (D1)
  filter(!estado %in% EXCLUIR_AGREG) %>%             # (D2)
  filter(!grupo_natural %in% EXCLUIR_AGREG) %>%      # (D2)
  filter(estado == ESTADO_OBJETIVO)

# --- SERIES DE SALIDA -------------------------------------------------------
# (a) Serie total estatal por año (todas las variables agregadas)
serie_total <- data_clean %>%
  group_by(year) %>%
  summarise(
    sup_sembrada = sum(sup_sembrada, na.rm = TRUE),
    produccion   = sum(produccion,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(nivel = "total")

# (b) Serie por grupo natural por año (para análisis de composición)
serie_grupo <- data_clean %>%
  group_by(year, grupo_natural) %>%
  summarise(
    sup_sembrada = sum(sup_sembrada, na.rm = TRUE),
    produccion   = sum(produccion,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(nivel = "grupo_natural")

# Unir en formato largo para un solo archivo reproducible
serie_estatal <- bind_rows(
  serie_total %>% mutate(grupo_natural = "TODOS"),
  serie_grupo
) %>%
  select(year, nivel, grupo_natural, sup_sembrada, produccion) %>%
  arrange(year, nivel, grupo_natural)

# --- VERIFICACIONES ---------------------------------------------------------
cat("=== VERIFICACIÓN SERIE ESTATAL QUERÉTARO ===\n")
cat("Años:", min(serie_estatal$year), "-", max(serie_estatal$year), "\n")

picos <- serie_total %>% filter(year %in% c(2003, 2010, 2022, 2024))
cat("\nSuperficie sembrada en años clave:\n")
for (i in seq_len(nrow(picos))) {
  cat(sprintf("  %d: %s ha\n", picos$year[i],
              format(round(picos$sup_sembrada[i]), big.mark = ",")))
}

caida <- (serie_total$sup_sembrada[serie_total$year == 2022] /
          serie_total$sup_sembrada[serie_total$year == 2010] - 1) * 100
cat(sprintf("\nCaída 2010 -> 2022: %.1f%%\n", caida))

# --- GUARDAR ----------------------------------------------------------------
write_csv(serie_estatal, OUTPUT)
cat("\nArchivo guardado:", OUTPUT, "\n")

# --- REPRODUCIBILIDAD -------------------------------------------------------
cat("\n=== sessionInfo() ===\n")
print(sessionInfo())
