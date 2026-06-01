# Trayectoria de la superficie sembrada en Querétaro (1981–2024)

Análisis reproducible de la superficie sembrada en el estado de Querétaro, México, entre 1981 y 2024, a nivel estatal y municipal. Este repositorio acompaña al artículo *"Trayectoria de la superficie sembrada en Querétaro y sus quiebres estructurales, 1981–2024"*, en preparación para la revista NTHE.

## Resumen del análisis

El estudio describe la evolución de la superficie sembrada en Querétaro e identifica sus quiebres estructurales mediante regresión segmentada (Bai–Perron). Examina la recomposición de los grupos de cultivo, el rendimiento del maíz por modalidad (riego y temporal) y la distribución municipal del cambio en la superficie.

## Estructura del repositorio

```
.
├── R/                                 Scripts de procesamiento y análisis (en orden de ejecución)
│   ├── 00_preparacion_base_estatal.R     Limpieza de la consulta cruda estatal → data_agregada_base.csv
│   ├── 01_limpieza_estatal.R             Serie anual de Querétaro a partir de la base agregada
│   ├── 02_analisis_estatal.R             Quiebres estructurales (BIC), Figuras 1 y 2, Tabla 1
│   ├── 03_modalidad_superficie.R         Superficie sembrada por modalidad (riego/temporal)
│   ├── 04_modalidad_rendimiento_maiz.R   Rendimiento del maíz por tipo y modalidad (Tabla 2)
│   └── 05_mapas_cambio_municipal.R       Mapas municipales de cambio en superficie (Figura 3)
├── data/                              Datos procesados (salidas de los scripts)
└── figures/                          Figuras finales del artículo
```

## Flujo de ejecución

Los scripts se ejecutan en orden numérico. La cadena de dependencias es:

```
Agro_1980-2024-ala.csv (crudo estatal, en Kaggle)
        │  00_preparacion_base_estatal.R
        ▼
data_agregada_base.csv  ← punto de partida del análisis (en Kaggle)
        │  01_limpieza_estatal.R
        ▼
serie_estatal_qro_1981-2024.csv
        │  02_analisis_estatal.R → Figuras 1-2, Tabla 1
        ▼
(análisis estatal)

dato_agricola_qro_municipal_riego.txt + _temporal.txt (crudos, en Kaggle)
        │  03, 04, 05
        ▼
series de modalidad, rendimiento del maíz (Tabla 2) y mapas (Figura 3)
```

### Contenido de `data/`

| Archivo | Generado por | Descripción |
|---|---|---|
| `serie_estatal_qro_1981-2024.csv` | script 01 | Serie anual de superficie por grupo de cultivo |
| `tabla_quiebres_estatal.csv` | script 02 | Cambios entre quiebres estructurales (Tabla 1) |
| `serie_modalidad_qro_2003-2024.csv` | script 03 | Superficie por modalidad, riego y temporal |
| `serie_rend_maiz_qro_2003-2024.csv` | script 04 | Rendimiento del maíz por tipo y modalidad (Tabla 2) |
| `tabla_cambio_municipal.csv` | script 05 | Cambio de superficie por municipio y modalidad |

### Contenido de `figures/`

| Archivo | Figura | Descripción |
|---|---|---|
| `fig1_serie_superficie_span030.png` | Figura 1 | Serie de superficie sembrada con quiebres |
| `fig2b_superficie_grupo_areas.png` | Figura 2 | Composición por grupo de cultivo (áreas apiladas) |
| `fig_mapas_cambio_modalidad.png` | Figura 3 | Cambio municipal de superficie por modalidad |

## Datos

Los datos provienen del **Sistema de Información Agroalimentaria de Consulta (SIACON-NG)** del Servicio de Información Agroalimentaria y Pesquera (SIAP-SADER), con fecha de descarga del 1 de diciembre de 2025.

- Nivel estatal con desglose por grupo de cultivo: 1980–2024.
- Nivel municipal con desglose por modalidad (riego y temporal): 2003–2024.

El análisis es reproducible de principio a fin. La consulta cruda estatal de SIACON (`Agro_1980-2024-ala.csv`) se procesa con el script `00_preparacion_base_estatal.R` para generar `data_agregada_base.csv`, punto de partida directo del análisis de Querétaro. Los datos municipales (.txt) se proporcionan en su forma cruda.

Todos los datos de entrada (la consulta cruda estatal, el archivo intermedio y los dos crudos municipales) están disponibles en [Kaggle](https://www.kaggle.com/datasets/chrisantics/superficie-agrcola-de-quertaro-1980-2024).

## Cómo reproducir el análisis

1. Clonar o descargar este repositorio.
2. Descargar los datos de entrada desde Kaggle y colocarlos en la carpeta del proyecto.
3. Abrir R o RStudio en la carpeta del proyecto.
4. Ejecutar los scripts de la carpeta `R/` en orden numérico (`00` → `05`).

Los scripts asumen que se ejecutan desde la carpeta del proyecto. En RStudio: *Session > Set Working Directory > To Source File Location*.

### Requisitos

- R (versión 4.0 o superior).
- Paquetes: `tidyverse`, `strucchange`, `scales`, `patchwork`, `mxmaps`.

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
