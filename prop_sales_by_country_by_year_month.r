# ==============================================================================
# Nombre del Script: prop_sales_by_country_by_year_month.R
# Descripción: Calcula la proporción de ventas por país agrupado por año y mes
#              (Periodo: 2013 - 2015) para Rossman Pharma.
# Salida: prop_sales_by_country_by_year_month.csv
# ==============================================================================

# 1. Cargar librerías necesarias
install.packages(c("dplyr", "lubridate", "tidyr"))
install.packages("readr")
library(dplyr)
library(lubridate)
library(tidyr)
library(readr)

# ------------------------------------------------------------------------------
# 2. CARGA DE DATOS 
# ------------------------------------------------------------------------------
df_ventas <- read_csv("ventas_pais.csv")

# ------------------------------------------------------------------------------
# 3. PROCESAMIENTO Y TRANSFORMACIÓN
# ------------------------------------------------------------------------------

# Paso A: Filtrar años, extraer Año/Mes y sumar las ventas totales por País/Mes
ventas_mensuales <- df_ventas %>%
  # Extraer año y mes de la fecha
  mutate(
    Year = year(Date),
    # El label=TRUE nos da el nombre del mes, abbr=FALSE lo da completo (ej. enero)
    Month = month(Date, label = TRUE, abbr = FALSE) 
  ) %>%
  # Filtrar estrictamente el periodo 2013-2015
  filter(Year %in% c(2013, 2014, 2015)) %>%
  # Agrupar por año, mes y país para sumar las ventas
  group_by(Year, Month, Country) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE), .groups = "drop")

# Paso B: Calcular la proporción por país respecto al total de ese mes
proporciones <- ventas_mensuales %>%
  group_by(Year, Month) %>%
  mutate(
    # Calcular porcentaje (Ventas del país / Ventas totales del mes)
    Proportion = Total_Sales / sum(Total_Sales),
    # Formatear el porcentaje a un texto legible (ej: "60.5%")
    Proportion_Pct = paste0(round(Proportion * 100, 1), "%")
  ) %>%
  ungroup()

# Paso C: Pivotar la tabla para que sea ancha (Wide Format)
# Esto dejará una fila por cada "Año-Mes" y las columnas serán los países
tabla_final <- proporciones %>%
  select(Year, Month, Country, Proportion_Pct) %>%
  pivot_wider(
    names_from = Country, 
    values_from = Proportion_Pct,
    values_fill = "0%" # Por si un país no tuvo ventas un mes
  ) %>%
  # Ordenar cronológicamente
  arrange(Year, match(Month, month.name))

# ------------------------------------------------------------------------------
# 4. EXPORTAR RESULTADOS
# ------------------------------------------------------------------------------
# Guardar la tabla en el archivo CSV requerido
nombre_archivo <- "prop_sales_by_country_by_year_month.csv"
write_csv(tabla_final, nombre_archivo)

# Mensaje de confirmación en consola
cat("El cálculo finalizó con éxito.\n")
cat("Se ha generado el archivo:", nombre_archivo, "\n\n")

# Mostrar una vista previa de los primeros registros en la consola
print(head(tabla_final))

