# Trayectoria de la superficie sembrada en Querétaro (1981–2024)

Análisis reproducible de la superficie sembrada en el estado de Querétaro, México, entre 1981 y 2024, a nivel estatal y municipal. Este repositorio acompaña al artículo *"Trayectoria de la superficie sembrada en Querétaro y sus quiebres estructurales, 1981–2024"*, en preparación para la revista NTHE.

## Resumen del análisis

El estudio describe la evolución de la superficie sembrada en Querétaro e identifica sus quiebres estructurales mediante regresión segmentada (Bai–Perron). Examina la recomposición de los grupos de cultivo, la superficie sembrada por modalidad (riego y temporal) y la distribución municipal del cambio en la superficie.

## Estructura del repositorio

```
.
├── R/                                 Scripts de procesamiento y análisis (en orden de ejecución)
│   ├── 00_preparacion_base_estatal.R     Limpieza de la consulta cruda estatal → raw_data_agregada_base.csv
│   ├── 01_limpieza_estatal.R             Serie anual de Querétaro a partir de la base agregada
│   ├── 02_analisis_estatal.R             Quiebres estructurales (BIC), Figuras 1 y 3, Tabla 1
│   ├── 03_mapas_estado_anios_clave.R     Mapas del estado en años clave (Figura 2)
│   ├── 04_modalidad_estatal.R            Superficie sembrada estatal por modalidad (Figura 4)
│   └── 05_mapas_municipales.R            Mapas municipales de superficie y cambio (Figura 5)
├── data/                              Datos procesados (salidas de los scripts)
└── figures/                          Figuras finales del artículo
```

## Flujo de ejecución

Los scripts se ejecutan en orden numérico. La cadena de dependencias es:

```
raw_Agro_estatal_1980-2024.csv (crudo estatal, en Kaggle)
        │  00_preparacion_base_estatal.R
        ▼
raw_data_agregada_base.csv  ← punto de partida del análisis (en Kaggle)
        │  01_limpieza_estatal.R
        ▼
serie_estatal_qro_1981-2024.csv
        │  02_analisis_estatal.R → Figuras 1 y 3, Tabla 1
        │  03_mapas_estado_anios_clave.R → Figura 2
        ▼
(análisis estatal)

raw_qro_estatal_riego.txt + raw_qro_estatal_temporal.txt (crudos, en Kaggle)
        │  04_modalidad_estatal.R → Figura 4
        ▼
serie estatal por modalidad (riego/temporal), 1981-2024

raw_qro_municipal_riego.txt + raw_qro_municipal_temporal.txt (crudos, en Kaggle)
        │  05_mapas_municipales.R → Figura 5
        ▼
mapas municipales de superficie (2003, 2024) y cambio porcentual
```

### Contenido de `data/`

| Archivo | Generado por | Descripción |
|---|---|---|
| `serie_estatal_qro_1981-2024.csv` | script 01 | Serie anual de superficie por grupo de cultivo |
| `tabla_quiebres_estatal.csv` | script 02 | Cambios entre quiebres estructurales (Tabla 1) |
| `serie_modalidad_estatal_1981-2024.csv` | script 04 | Superficie estatal por modalidad, riego y temporal |

### Contenido de `figures/`

| Archivo | Figura | Descripción |
|---|---|---|
| `fig1_serie_superficie_span030.png` | Figura 1 | Serie de superficie sembrada con quiebres estructurales |
| `fig_mapas_estado_anios_clave.png` | Figura 2 | Superficie sembrada estatal en cuatro años clave |
| `fig2b_superficie_grupo_areas.png` | Figura 3 | Composición por grupo de cultivo (áreas apiladas) |
| `fig_modalidad_estatal.png` | Figura 4 | Superficie sembrada por modalidad (riego/temporal) |
| `fig_mapas_municipales_2003_2024.png` | Figura 5 | Superficie municipal (2003, 2024) y cambio porcentual |

## Datos

Los datos provienen del **Sistema de Información Agroalimentaria de Consulta (SIACON-NG)** del Servicio de Información Agroalimentaria y Pesquera (SIAP-SADER), con fecha de descarga del 1 de diciembre de 2025.

- Nivel estatal con desglose por grupo de cultivo: 1980–2024.
- Nivel estatal con desglose por modalidad (riego y temporal): 1980–2024.
- Nivel municipal con desglose por modalidad (riego y temporal): 2003–2024.

El análisis es reproducible de principio a fin. La consulta cruda estatal de SIACON (`raw_Agro_estatal_1980-2024.csv`) se procesa con el script `00_preparacion_base_estatal.R` para generar `raw_data_agregada_base.csv`, punto de partida directo del análisis de Querétaro. Los archivos de modalidad (estatal y municipal) se proporcionan en su forma cruda (prefijo `raw_`).

Todos los datos de entrada están disponibles en [Kaggle](https://www.kaggle.com/datasets/chrisantics/superficie-agrcola-de-quertaro-1980-2024), que incluye tanto los archivos crudos (`raw_`) como versiones limpias en formato tidy.

## Cómo reproducir el análisis

1. Clonar o descargar este repositorio.
2. Descargar los datos de entrada desde Kaggle y colocarlos en la carpeta del proyecto.
3. Abrir R o RStudio en la carpeta del proyecto.
4. Ejecutar los scripts de la carpeta `R/` en orden numérico (`00` → `05`).

Los scripts asumen que se ejecutan desde la carpeta del proyecto. En RStudio: *Session > Set Working Directory > To Source File Location*.

### Requisitos

- R (versión 4.0 o superior).
- Paquetes: `tidyverse`, `strucchange`, `scales`, `patchwork`, `ggrepel`, `sf`, `mxmaps`.

El paquete `mxmaps` no está en CRAN; se instala desde GitHub:

```r
install.packages("remotes")
remotes::install_github("diegovalle/mxmaps")
```

Cada script imprime `sessionInfo()` al finalizar para documentar las versiones utilizadas.

## Cita

Si utilizas este material, por favor cita el repositorio y el artículo asociado *(referencia completa pendiente de publicación)*.

## Licencia

El código se distribuye bajo licencia MIT. Los datos procesados y la documentación, bajo licencia CC BY 4.0.
