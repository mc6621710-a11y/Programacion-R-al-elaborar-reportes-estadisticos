# ==============================================================================
# Nombre del Script: top_10_performance_stores_by_country.R
# Descripción: Filtra el desempeño de los últimos 3 meses de la base de datos.
#              Determina las tiendas en la intersección del Top 10 en Ventas ($)
#              y Top 19 en Clientes por cada país, graficando sus promedios.
# Salida: top_10_performance_stores_by_country.png
# ==============================================================================

# 1. Cargar librerías críticas de procesamiento y graficación
# Instalar si faltan: install.packages(c("dplyr", "readr", "lubridate", "tidyr", "ggplot2"))
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(ggplot2)

# 2. Carga de datos unificada
if (file.exists("ventas_pais.csv")) {
  df_ventas <- read_csv("ventas_pais.csv")
} else {
  stop("Error: El archivo 'ventas_pais.csv' no se encuentra en el directorio actual.")
}

# 3. Delimitación temporal (3 meses hacia atrás desde la última fecha registrada)
ultima_fecha <- max(df_ventas$Date, na.rm = TRUE)
fecha_inicio <- ultima_fecha %m-% months(3)

cat("Rango de análisis establecido desde:", as.character(fecha_inicio), 
    "hasta:", as.character(ultima_fecha), "\n")

# 4. Agrupación y Cálculo de Filtros Cruzados (Top Histórico vs Promedios)
tiendas_rendimiento <- df_ventas %>%
  # Filtrar estrictamente la ventana temporal activa
  filter(Date >= fecha_inicio & Date <= ultima_fecha) %>%
  # Consolidar métricas métricas totales (para rankings) y promedios (para visualización)
  group_by(Country, Store) %>%
  summarise(
    Total_Sales     = sum(Sales, na.rm = TRUE),
    Total_Customers = sum(Customers, na.rm = TRUE),
    Avg_Sales       = mean(Sales, na.rm = TRUE),
    Avg_Customers   = mean(Customers, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Agrupar por país para aplicar los rankings independientes de mercado
  group_by(Country) %>%
  mutate(
    rank_ventas   = min_rank(desc(Total_Sales)),
    rank_clientes = min_rank(desc(Total_Customers))
  ) %>%
  # Intersección estricta: Top 10 monetario Y Top 19 en volumen de clientes
  filter(rank_ventas <= 10 & rank_clientes <= 19) %>%
  ungroup()

# 5. Transformación a formato largo (Long Format) para optimizar la visualización dual
plot_data <- tiendas_rendimiento %>%
  select(Country, Store, Avg_Sales, Avg_Customers) %>%
  pivot_longer(
    cols = c(Avg_Sales, Avg_Customers),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  # Renombrar variables para etiquetas profesionales en la gráfica
  mutate(
    Metric = ifelse(Metric == "Avg_Sales", "Venta Promedio ($)", "Clientes Promedio")
  )

# 6. Construcción y Estilizado del Gráfico de Alto Rendimiento
p <- ggplot(plot_data, aes(x = reorder(Store, Value), y = Value, fill = Metric)) +
  # Barras horizontales limpias
  geom_bar(stat = "identity", alpha = 0.85, width = 0.7, show.legend = FALSE) +
  
  # Distribución matricial inteligente: Filas por País, Columnas por Métrica
  facet_grid(Country ~ Metric, scales = "free") +
  
  # Rotación de ejes para lectura idónea de nombres de tiendas
  coord_flip() +
  
  # Paleta de colores sobria y corporativa
  scale_fill_manual(values = c("Venta Promedio ($)" = "#2c3e50", "Clientes Promedio" = "#16a085")) +
  
  # Textos y títulos ejecutivos
  labs(
    title = "Top Performance Stores por País (Intersección Ventas y Clientes)",
    subtitle = paste("Filtro operativo: Ventas en Top 10 y Clientes en Top 19 durante los últimos 3 meses anteriores a", ultima_fecha),
    x = "Identificador de Tienda (Store ID)",
    y = "Métricas Promedio de Operación",
    caption = "Fuente: Base de Datos Rossmann Pharma (Segmentación cruzada por volumen y valor monetario)"
  ) +
  
  # Ajustes de interfaz y diseño de fondo
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#2c3e50"),
    plot.subtitle = element_text(size = 9.5, color = "#7f8c8d"),
    strip.text = element_text(face = "bold", size = 10, color = "white"),
    strip.background = element_rect(fill = "#34495e"),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 8, face = "bold"),
    plot.caption = element_text(size = 8, color = "#95a5a6", face = "italic")
  )

# 7. Guardar la visualización en la resolución requerida
nombre_grafico <- "top_10_performance_stores_by_country.png"
ggsave(
  filename = nombre_grafico,
  plot = p,
  width = 10,
  height = 8,
  dpi = 300
)

cat("Proceso completado con éxito. Se ha exportado la gráfica:", nombre_grafico, "\n")