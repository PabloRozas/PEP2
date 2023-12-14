# Se lee el csv con codificaciojn latin1
data <- read.csv2("EP08 Datos CASEN 2017.csv", fileEncoding = "latin1")

# Se crea un data frame con las columnas r14 r12b region
# (filtrada por Región de Tarapacá)
datos_interes <- data.frame(data$r14, data$r12b, data$region)

# Se filtran los datos por Región de Tarapacá
datos_interes <- datos_interes[datos_interes$region == "Región de Tarapacá", ]


# Variable para ver los nombres de las columnas
names(datos_interes)

# Ver los primeros 5 elementos de data
head(data)