# ============================================================================
# 02_analisis_estatal.R
# Proyecto: Capacidad productiva agrícola de Querétaro (1981-2024)
# ----------------------------------------------------------------------------
# OBJETIVO: Producir los resultados y figuras del artículo NTHE a partir de
#           la serie estatal limpia de Querétaro.
#
# INPUT:  serie_estatal_qro_1981-2024.csv   (salida de 00_limpieza_estatal.R)
# OUTPUTS:
#   fig1_serie_superficie_span030.png      (serie + LOESS + quiebres)
#   fig2a_superficie_grupo_lineas.png      (sup. sembrada por grupo, líneas)
#   fig2b_superficie_grupo_areas.png       (sup. sembrada por grupo, áreas apiladas)
#   fig2c_superficie_grupo_relativa.png    (participación relativa por grupo, %)
#   fig3_superficie_vs_produccion.png      (desacople sup. vs producción)
#   tabla_quiebres_estatal.csv             (años de quiebre y cambios por fase)
#
# FLUJO DE TRABAJO (IMPORTANTE):
#   PARTE A — Diagnóstico: imprime tabla BIC y grafica la curva BIC.
#             El usuario OBSERVA y decide el número óptimo de quiebres.
#   PARTE B — Resultados: el usuario fija K_QUIEBRES y se generan las figuras.
#
# DECISIONES METODOLÓGICAS:
#   (D1) Regresión segmentada Superficie ~ year (Bai-Perron, strucchange).
#   (D2) Tamaño mínimo de segmento h = 0.15 de la serie.
#   (D3) Selección del número de quiebres por BIC mínimo (decisión del usuario
#        tras observar la PARTE A).
#   (D4) LOESS con span = 0.30 para la curva de tendencia.
#   (D5) La Figura 2 analiza SUPERFICIE SEMBRADA por grupo natural (variable
#        aditiva entre cultivos). NO se calcula rendimiento por grupo: el
#        rendimiento implícito (producción/superficie) agregado mezcla cultivos
#        con escalas ton/ha no comparables (p.ej. alfalfa verde vs grano de
#        temporal), por lo que no es un indicador interpretable a nivel grupo.
#        El rendimiento solo se usa a nivel total agregado en la Figura 3.
#
# Licencia código: MIT.
# ============================================================================

library(tidyverse)
library(strucchange)
library(scales)

# --- PARÁMETROS -------------------------------------------------------------
INPUT     <- "serie_estatal_qro_1981-2024.csv"
H_MINIMO  <- 0.15        # (D2)
MAX_BREAKS <- 5          # número máximo de quiebres a evaluar en el diagnóstico

# >>> El usuario fija este valor DESPUÉS de observar la PARTE A <<<
K_QUIEBRES <- NA         # p.ej. K_QUIEBRES <- 2   (déjalo en NA hasta decidir)

# --- CARGA ------------------------------------------------------------------
serie_estatal <- read_csv(INPUT, col_types = cols())

# Serie total (nivel = "total")
serie_total <- serie_estatal %>%
  filter(nivel == "total") %>%
  arrange(year) %>%
  mutate(sup_millones = sup_sembrada / 1e6)

cat("=== SERIE CARGADA ===\n")
cat("Años:", min(serie_total$year), "-", max(serie_total$year),
    "| n =", nrow(serie_total), "\n")

# ============================================================================
# PARTE A — DIAGNÓSTICO: ¿cuántos quiebres? (OBSERVAR antes de decidir)
# ============================================================================
cat("\n========== PARTE A: DIAGNÓSTICO DE QUIEBRES ==========\n")

bp_test <- breakpoints(sup_sembrada ~ year,
                       data = serie_total,
                       h = H_MINIMO,
                       breaks = MAX_BREAKS)

resumen <- summary(bp_test)
bic_vals <- resumen$RSS["BIC", ]   # vector BIC indexado por número de quiebres

# Tabla BIC legible: para cada k, BIC y años de quiebre
cat("\n--- Tabla BIC por número de quiebres ---\n")
tabla_bic <- tibble(k = as.integer(names(bic_vals)), BIC = as.numeric(bic_vals)) %>%
  rowwise() %>%
  mutate(
    anios_quiebre = if (k == 0) "(ninguno)" else {
      idx <- breakpoints(bp_test, breaks = k)$breakpoints
      paste(serie_total$year[idx], collapse = ", ")
    }
  ) %>%
  ungroup()
print(tabla_bic, n = Inf)

k_sugerido <- tabla_bic$k[which.min(tabla_bic$BIC)]
cat("\n>>> BIC mínimo sugiere k =", k_sugerido, "quiebres <<<\n")
cat(">>> Observa la curva BIC y la tabla, luego fija K_QUIEBRES arriba. <<<\n")

# Gráfica de la curva BIC (para inspección visual)
png("fig_diagnostico_BIC.png", width = 1600, height = 1000, res = 200)
plot(bp_test, main = "Selección de número de quiebres (BIC) — Querétaro")
dev.off()
cat("\nFigura diagnóstico guardada: fig_diagnostico_BIC.png\n")

# ============================================================================
# PARTE B — RESULTADOS FINALES (requiere K_QUIEBRES definido)
# ============================================================================
if (is.na(K_QUIEBRES)) {
  cat("\n========== PARTE B en pausa ==========\n")
  cat("Define K_QUIEBRES (línea de parámetros) con el valor que elegiste y",
      "vuelve a correr el script.\n")
} else {
  cat("\n========== PARTE B: RESULTADOS CON K_QUIEBRES =", K_QUIEBRES, "==========\n")

  # --- Años de quiebre seleccionados ----------------------------------------
  if (K_QUIEBRES > 0) {
    idx_q <- breakpoints(bp_test, breaks = K_QUIEBRES)$breakpoints
    anios_quiebre <- serie_total$year[idx_q]
  } else {
    anios_quiebre <- integer(0)
  }
  cat("Años de quiebre:", paste(anios_quiebre, collapse = ", "), "\n")

  # --- Tabla de cambios por fase --------------------------------------------
  limites <- c(min(serie_total$year), anios_quiebre, max(serie_total$year))
  tabla_fases <- tibble(
    inicio = head(limites, -1),
    fin    = tail(limites, -1)
  ) %>%
    rowwise() %>%
    mutate(
      sup_inicio = serie_total$sup_sembrada[serie_total$year == inicio],
      sup_fin    = serie_total$sup_sembrada[serie_total$year == fin],
      cambio_pct = (sup_fin / sup_inicio - 1) * 100,
      tipo = case_when(cambio_pct >  2 ~ "Incremento",
                       cambio_pct < -2 ~ "Decremento",
                       TRUE            ~ "Meseta")
    ) %>%
    ungroup()
  cat("\n--- Cambios por fase ---\n")
  print(tabla_fases)
  write_csv(tabla_fases, "tabla_quiebres_estatal.csv")

  # --- FIGURA 1: serie + LOESS (span 0.30) + quiebres -----------------------
  fig1 <- ggplot(serie_total, aes(year, sup_millones)) +
    geom_line(color = "black", linewidth = 0.9, alpha = 0.75) +
    geom_point(data = filter(serie_total, year %in% limites),
               color = "black", fill = "#D55E00", shape = 21, size = 2.5) +
    geom_smooth(method = "loess", span = 0.30, se = FALSE,
                color = "#0072B2", linewidth = 0.9) +
    scale_y_continuous(labels = comma_format(accuracy = 0.01)) +
    scale_x_continuous(breaks = seq(1981, 2024, 3)) +
    labs(x = "Año", y = "Superficie sembrada (millones de ha)") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank())

  if (length(anios_quiebre) > 0) {
    fig1 <- fig1 +
      geom_vline(xintercept = anios_quiebre, linetype = "dashed",
                 color = "gray50", linewidth = 0.6)
  }

  ggsave("fig1_serie_superficie_span030.png", fig1,
         width = 10, height = 5.5, dpi = 300)
  cat("\nFigura 1 guardada (LOESS span = 0.30).\n")

  # --- FIGURA 2: superficie sembrada por grupo natural ----------------------
  # NOTA (D5): se analiza SUPERFICIE (aditiva), no rendimiento por grupo.
  serie_grupo <- serie_estatal %>%
    filter(nivel == "grupo_natural", sup_sembrada > 0)

  # Grupos de mayor superficie histórica; el resto se agrupa en "Otros"
  grupos_top <- serie_grupo %>%
    group_by(grupo_natural) %>%
    summarise(sup_media = mean(sup_sembrada), .groups = "drop") %>%
    slice_max(sup_media, n = 6) %>%
    pull(grupo_natural)

  serie_grupo_plot <- serie_grupo %>%
    mutate(grupo = if_else(grupo_natural %in% grupos_top,
                           grupo_natural, "Otros")) %>%
    group_by(year, grupo) %>%
    summarise(sup_sembrada = sum(sup_sembrada), .groups = "drop")

  paleta <- c("#0072B2","#D55E00","#009E73","#CC79A7",
              "#E69F00","#56B4E9","#999999")

  # (2a) Líneas: superficie absoluta por grupo
  fig2a <- ggplot(serie_grupo_plot,
                  aes(year, sup_sembrada / 1e3, color = grupo)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = paleta) +
    scale_x_continuous(breaks = seq(1981, 2024, 6)) +
    labs(x = "Año", y = "Superficie sembrada (miles de ha)",
         color = "Grupo natural") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
  ggsave("fig2a_superficie_grupo_lineas.png", fig2a,
         width = 10, height = 5.5, dpi = 300)

  # (2b) Áreas apiladas: composición del total año a año
  fig2b <- ggplot(serie_grupo_plot,
                  aes(year, sup_sembrada / 1e3, fill = grupo)) +
    geom_area(alpha = 0.9) +
    scale_fill_manual(values = paleta) +
    scale_x_continuous(breaks = seq(1981, 2024, 6)) +
    labs(x = "Año", y = "Superficie sembrada (miles de ha)",
         fill = "Grupo natural") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
  ggsave("fig2b_superficie_grupo_areas.png", fig2b,
         width = 10, height = 5.5, dpi = 300)

  # (2c) Participación relativa (%): cómo cambia el peso de cada grupo
  fig2c <- serie_grupo_plot %>%
    group_by(year) %>%
    mutate(participacion = sup_sembrada / sum(sup_sembrada) * 100) %>%
    ungroup() %>%
    ggplot(aes(year, participacion, fill = grupo)) +
    geom_area(alpha = 0.9) +
    scale_fill_manual(values = paleta) +
    scale_x_continuous(breaks = seq(1981, 2024, 6)) +
    labs(x = "Año", y = "Participación en superficie (%)",
         fill = "Grupo natural") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
  ggsave("fig2c_superficie_grupo_relativa.png", fig2c,
         width = 10, height = 5.5, dpi = 300)

  cat("Figuras 2a/2b/2c guardadas (superficie por grupo: líneas, áreas, %).\n")

  # --- FIGURA 3: desacople superficie vs producción (índice base 1981=100) --
  fig3_data <- serie_total %>%
    mutate(
      sup_idx  = sup_sembrada / sup_sembrada[year == 1981] * 100,
      prod_idx = produccion   / produccion[year == 1981]   * 100
    ) %>%
    select(year, sup_idx, prod_idx) %>%
    pivot_longer(-year, names_to = "serie", values_to = "indice") %>%
    mutate(serie = recode(serie,
                          sup_idx  = "Superficie sembrada",
                          prod_idx = "Producción"))

  fig3 <- ggplot(fig3_data, aes(year, indice, color = serie)) +
    geom_hline(yintercept = 100, color = "gray70", linewidth = 0.4) +
    geom_line(linewidth = 0.9) +
    scale_color_manual(values = c("Superficie sembrada" = "#D55E00",
                                  "Producción" = "#009E73")) +
    scale_x_continuous(breaks = seq(1981, 2024, 6)) +
    labs(x = "Año", y = "Índice (1981 = 100)", color = NULL,
         subtitle = "Desacople entre superficie y producción") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())

  ggsave("fig3_superficie_vs_produccion.png", fig3,
         width = 10, height = 5.5, dpi = 300)
  cat("Figura 3 guardada: desacople superficie vs producción.\n")

  cat("\n=== PARTE B COMPLETA ===\n")
}

# --- REPRODUCIBILIDAD -------------------------------------------------------
cat("\n=== sessionInfo() ===\n")
print(sessionInfo())
