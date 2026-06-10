# ==============================================================================
# Nombre del Script: prop_changes_sales_by_country_by_year_month.R
# Descripción: Calcula el cambio porcentual mensual (crecimiento/decrecimiento) 
#              de las ventas de cada país con respecto al mes anterior (2013-2015).
# Salida: prop_changes_sales_by_country_by_year_month.csv
# ==============================================================================

# 1. Cargar librerías necesarias
library(dplyr)
library(lubridate)
library(tidyr)
library(readr)

# 2. Carga de datos
# Se asume que 'ventas_pais.csv' está en el mismo directorio de trabajo
if (file.exists("ventas_pais.csv")) {
  df_ventas <- read_csv("ventas_pais.csv")
} else {
  stop("Error: El archivo 'ventas_pais.csv' no se encuentra en el directorio actual.")
}

# 3. Procesamiento y cálculo de la tasa de crecimiento mensual
cambios_mensuales <- df_ventas %>%
  # Extraer componentes de fecha necesarios para agrupar y ordenar
  mutate(
    Year = year(Date),
    Month_Num = month(Date), # Mantener número de mes para ordenación estricta
    Month_Name = month(Date, label = TRUE, abbr = FALSE)
  ) %>%
  # Filtrar el periodo de análisis
  filter(Year %in% c(2013, 2014, 2015)) %>%
  # Agrupar para consolidar las ventas mensuales totales por año, mes y país
  group_by(Year, Month_Num, Month_Name, Country) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE), .groups = "drop") %>%
  # ORDENAR de forma crítica: Primero por país, luego cronológicamente
  arrange(Country, Year, Month_Num) %>%
  # Agrupar por país de forma aislada para que el cálculo del mes anterior no se mezcle
  group_by(Country) %>%
  mutate(
    # Obtener la venta del mes inmediato anterior (t - 1)
    Sales_Previous_Month = lag(Total_Sales),
    # Aplicar fórmula del cambio porcentual: ((Venta_Actual - Venta_Anterior) / Venta_Anterior) * 100
    Pct_Change = ((Total_Sales - Sales_Previous_Month) / Sales_Previous_Month) * 100,
    # Formatear el resultado como texto. El primer mes de 2013 no tiene mes anterior, asignamos "N/A"
    Pct_Change_Formated = ifelse(is.na(Pct_Change), "N/A", paste0(round(Pct_Change, 1), "%"))
  ) %>%
  ungroup()

# 4. Pivotar a formato ancho (Wide Format) para cumplir con la estructura de la tabla
tabla_final_cambios <- cambios_mensuales %>%
  select(Year, Month_Name, Month_Num, Country, Pct_Change_Formated) %>%
  pivot_wider(
    names_from = Country,
    values_from = Pct_Change_Formated
  ) %>%
  # Ordenar la estructura final cronológicamente por Año y Mes
  arrange(Year, Month_Num) %>%
  # Remover la columna numérica auxiliar de ordenación y renombrar
  select(-Month_Num) %>%
  rename(Month = Month_Name)

# 5. Exportar resultados a la ruta local
nombre_archivo_salida <- "prop_changes_sales_by_country_by_year_month.csv"
write_csv(tabla_final_cambios, nombre_archivo_salida)

# Mensaje de control e impresión de los primeros registros
cat("¡Procesamiento exitoso!\n")
cat("Se ha guardado el reporte en:", nombre_archivo_salida, "\n\n")
print(head(tabla_final_cambios, 6))