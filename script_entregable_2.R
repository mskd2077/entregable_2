# Instalar y cargar el paquete necesario para leer archivos de Excel
install.packages("readxl")
library(readxl)

# Cargar los datos desde el archivo Excel
datos_sismicos <- read_excel("Catalogo1960_2023.xlsx")

# Convertir la columna FECHA_UTC a formato de fecha
datos_sismicos$FECHA_UTC <- as.Date(as.character(datos_sismicos$FECHA_UTC), format = "%Y%m%d")

# Gráfico 1: Histograma de Magnitudes
hist(datos_sismicos$MAGNITUD, main = "Distribución de Magnitud de Sismos", xlab = "Magnitud")

# Gráfico 2: Diagrama de Dispersión de Profundidad vs. Magnitud
plot(datos_sismicos$PROFUNDIDAD, datos_sismicos$MAGNITUD, 
     xlab = "Profundidad", ylab = "Magnitud", 
     main = "Relación entre Profundidad y Magnitud")

# Gráfico 3: Serie Temporal de Sismos
# Contar la cantidad de sismos por año
sismos_por_anio <- table(format(datos_sismicos$FECHA_UTC, "%Y"))

# Graficar la serie temporal de sismos
plot(as.numeric(names(sismos_por_anio)), sismos_por_anio, 
     type = "o", xlab = "Año", ylab = "Cantidad de Sismos",
     main = "Serie Temporal de Sismos")

# Modelo Predictivo: Regresión Lineal
modelo <- lm(MAGNITUD ~ PROFUNDIDAD, data = datos_sismicos)

# Realizar predicciones con el modelo
profundidad_nueva <- seq(min(datos_sismicos$PROFUNDIDAD), max(datos_sismicos$PROFUNDIDAD), length.out = 100)
predicciones <- predict(modelo, newdata = data.frame(PROFUNDIDAD = profundidad_nueva))

# Visualización del Modelo: Diagrama de Dispersión con Predicciones
plot(datos_sismicos$PROFUNDIDAD, datos_sismicos$MAGNITUD, 
     xlab = "Profundidad", ylab = "Magnitud", 
     main = "Relación entre Profundidad y Magnitud con Predicciones")
lines(profundidad_nueva, predicciones, col = "red", lwd = 2)
