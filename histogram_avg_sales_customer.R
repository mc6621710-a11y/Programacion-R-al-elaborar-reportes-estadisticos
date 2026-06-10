# ==============================================================================
# Nombre del Script: histogram_avg_sales_customer.R
# Descripción: Genera un histograma de frecuencias de la variable 'avg_sales_customer'
#              (Sales/Customers) para España y Francia, mostrando el promedio y
#              los intervalos de la regla empírica débil (Chebyshev) y clásica (Normal) para el 95%.
# Salida: histogram_avg_sales_customer.png
# ==============================================================================

# 1. Cargar librerías necesarias
# Instalar si faltan: install.packages(c("dplyr", "readr", "ggplot2"))
library(dplyr)
library(readr)
library(ggplot2)

# 2. Carga de datos
if (file.exists("ventas_pais.csv")) {
  df_ventas <- read_csv("ventas_pais.csv")
} else {
  stop("Error: El archivo 'ventas_pais.csv' no se encuentra en el directorio de trabajo.")
}

# 3. Filtrado por países (España y Francia) y cálculo de la variable meta
df_analisis <- df_ventas %>%
  filter(Country %in% c("España", "Francia")) %>%
  mutate(avg_sales_customer = Sales / Customers) %>%
  # Limpieza de nulos o infinitos preventivos
  filter(!is.na(avg_sales_customer) & !is.infinite(avg_sales_customer))

# 4. Cálculo de métricas estadísticas fundamentales
media_avg <- mean(df_analisis$avg_sales_customer, na.rm = TRUE)
sd_avg    <- sd(df_analisis$avg_sales_customer, na.rm = TRUE)

# A. Regla Empírica Débil (Teorema de Chebyshev para el 95% de cobertura)
# Fórmula de Chebyshev: 1 - 1/k^2 = 0.95 -> k = sqrt(1 / 0.05) = sqrt(20) ≈ 4.47
k_debil <- sqrt(20)
lim_inf_debil <- media_avg - (k_debil * sd_avg)
lim_sup_debil <- media_avg + (k_debil * sd_avg)

# B. Regla Empírica Clásica / Fuerte (Distribución Normal para el 95% de cobertura)
# Fórmula clásica de la regla empírica: mu ± 2 * sigma
lim_inf_normal <- media_avg - (2 * sd_avg)
lim_sup_normal <- media_avg + (2 * sd_avg)

# 5. Diseño y construcción del histograma con ggplot2
p <- ggplot(df_analisis, aes(x = avg_sales_customer)) +
  # Histograma de frecuencias absolutas estilizado
  geom_histogram(bins = 30, fill = "#2c3e50", color = "white", alpha = 0.85) +
  
  # Línea representativa del Promedio (Media Aritmética)
  geom_vline(aes(xintercept = media_avg, color = "Promedio"), linetype = "solid", linewidth = 1.2) +
  
  # Líneas de los límites de la Regla Empírica Débil (Chebyshev)
  geom_vline(aes(xintercept = max(0, lim_inf_debil), color = "Regla Débil (Chebyshev 95%)"), linetype = "dotdash", linewidth = 1) +
  geom_vline(aes(xintercept = lim_sup_debil, color = "Regla Débil (Chebyshev 95%)"), linetype = "dotdash", linewidth = 1) +
  
  # Líneas de los límites de la Regla Empírica Clásica (Normal)
  geom_vline(aes(xintercept = max(0, lim_inf_normal), color = "Regla Clásica (Normal 95%)"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = lim_sup_normal, color = "Regla Clásica (Normal 95%)"), linetype = "dashed", linewidth = 1) +
  
  # Paleta de colores formal para la leyenda integrada
  scale_color_manual(name = "Métricas e Intervalos", values = c(
    "Promedio" = "#27ae60",
    "Regla Débil (Chebyshev 95%)" = "#e74c3c",
    "Regla Clásica (Normal 95%)" = "#2980b9"
  )) +
  
  # Textos informativos y etiquetas profesionales
  labs(
    title = "Distribución de Ventas Promedio por Cliente (avg_sales_customer)",
    subtitle = "Análisis acotado a los mercados de España y Francia (Periodo 2013-2015)",
    x = "Ticket Promedio por Cliente (Sales / Customers)",
    y = "Frecuencia Absoluta (Número de Transacciones)",
    caption = paste0("Estadísticos muestrales:\n",
                     "• Media (µ) = ", round(media_avg, 2), " | Desviación Estándar (σ) = ", round(sd_avg, 2), "\n",
                     "• Intervalo Regla Débil (Chebyshev 95%): [", round(max(0, lim_inf_debil), 2), ", ", round(lim_sup_debil, 2), "]\n",
                     "• Intervalo Regla Clásica (Normal 95%): [", round(max(0, lim_inf_normal), 2), ", ", round(lim_sup_normal), "]")
  ) +
  
  # Ajustes finos de tipografía y layout limpio
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, size = 8.5, color = "#34495e", face = "italic", lineheight = 1.2)
  )

# 6. Exportación de la gráfica a formato PNG de alta resolución
ggsave(
  filename = "histogram_avg_sales_customer.png",
  plot = p,
  width = 8.5,
  height = 5.5,
  dpi = 300
)

cat("Ejecución finalizada con éxito.\nSe ha creado la figura 'histogram_avg_sales_customer.png'.\n")
