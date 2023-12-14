#Equipo 1
#Integrantes:
#-Daniel Eguiluz
#-Pablo Macuada
#-Argenis Benitez

library(ggplot2)
library(ggpubr)

# Leer el csv EP01 Datos Casen 2017.csv y crear un data frame con las variables
datos <- read.csv("EP02 Datos.csv", header = TRUE, sep = ";", dec = ",")

# Descripción de los datos:
# id: identificador único para cada atleta
# raza: raza del atleta (categórica: Blanca, Negra, Oriental)
# previo: mejor tiempo registrado por el atleta antes de ingresar al programa de entrenamiento (numérica, en segundos)
# posterior: mejor tiempo registrado por el atleta durante los primeros 6 meses del programa de entrenamiento (numérica, en segundos)

# PREGUNTAS
# 1)
# El Comité Olímpico cree que el mejor tiempo medio de los atletas de raza negra después de 
# ingresar al programa de entrenamiento es superior a 12,4 segundos. ¿Soportan los datos esta afirmación?

# Se filtran los datos de los atletas de raza negra y se crea un data frame con estos datos
datos_raza_negra <- datos[datos$Raza == "Negra",]
datos_post_raza <- datos_raza_negra$Posterior

#COMPROBACION DE QUE LAS OBSERVACIONES SON INDEPENDIENTES ENTRE SI

#Como las muestras fueron elegidas al azar, se puede asumir que son independientes; como hay muchas (posiblemente infinitas) posibles instancias en la población, podemos asumir con seguridad que la
#muestra incluye menos del 10 % de la población

# COMPROBACIÓN DE SUPUESTO DE NORMALIDAD

# Usando grafico QQ-plot:
# Verificar si la distribución se acerca a la normal .
g <- ggqqplot(data = datos_raza_negra,
  x = "Posterior",
  color = " steelblue ",
  xlab = "Teórico ",
  ylab = " Muestra ",
  title = "Grá fico Q-Q muestra v/s distr . normal ")
plot(g)

#El gráfico realizado muestra que es válido suponer una distribución cercana a la normal. Si bien
#los puntos de la muestra no forman una recta, y se observan valores atípicos, estos
#no se alejan demasiado de la region aceptable, por lo que se puede aceptar la hipotesis de
#que los datos provienen distribución es normal.

#Como una segunda comprobación, se realiza el test de shapiro
normalidad <- shapiro.test ( datos_post_raza )
print(normalidad)
#El test de shapiro muestra un p-value de 0.2225 lo cual es mayor a 0.05, por lo que se acepta la hipotesis nula
#de que los datos provienen de una distribución normal.



#FORMULAR HIPOTESIS###
#Hipótesis nula (Ho): El mejor tiempo medio de los atletas de raza negra después de ingresar al programa de entrenamiento es igual a 12,4 segundos.
#Hipótesis alternativa (Ha): El mejor tiempo medio de los atletas de raza negra después de ingresar al programa de entrenamiento es superior a 12,4 segundos.

# T - TEST
valor_nulo=12.4
alfa=0.05

prueba <- t.test ( datos_post_raza ,
alternative = "greater",
mu = valor_nulo ,
conf.level =1 - alfa)
print(prueba)

# 2)
# ¿Sugieren los datos que la mejor marca de los atletas de raza blanca se reduce en 
# promedio 3,5 segundos tras el entrenamiento?

# 3)
# ¿Es posible afirmar que, en promedio, los atletas de raza blanca superaban a 
# los de raza oriental por menos de 3 segundos antes del entrenamiento?

# 4)
# ¿Será cierto que hay más atletas de raza oriental que, en promedio, 
# redujeron sus marcas en al menos 4,3 segundos que atletas de raza negra 
# que lo hicieron en al menos 1,4 segundos?

# Medida estadística seleccionada: Frecuencia y Proporción
# Se utiliza la frecuencia y la proporción ya que se esta trabajando con un tipo de dato cualitativo, en este caso la provincia, y se quiere saber la cantidad de personas encuestadas en cada provincia de la RM.
# Se puede notar de la tabla de frecuencia y proporción que se encuestaron más personas en la provincia de "Santiago" en relación a las demás provincias de la RM.

# Se crean las variables frec y prop, que corresponden a la frecuencia y proporción de las provincias de la RM respectivamente.
frec <- as.numeric(table(datos$provincia))
prop <- as.numeric(table(datos$provincia)/sum(table(datos$provincia)))*100
cat(prop)

# Se toman los valores de la columna provincia y se eliminan los valores repetidos
provincia <- unique(datos$provincia)

# Se ordenan las provincias de forma que se ajusten a la tabla de frecuencia
provincia_ordenada <- sort(provincia)

# Se crea una dataframe con las variables provincia_ordenada, frec y prop
tabla_frec <- data.frame(Provincia = provincia_ordenada, Frecuencia = frec, Proporcion = prop)

# Se imprime la tabla de frecuencia y proporción
knitr::kable(tabla_frec)

# Gráfico:
graf2 <- ggplot(datos, aes(x = provincia)) +
  geom_bar() + # Se utiliza geom_bar() ya que se esta trabajando con un tipo de dato cualitativo
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5, size=3) + # Se utiliza geom_text() para agregar la frecuencia de cada barra
  labs(x = "Provincia", y = "Cantidad de encuestado") + # Se agregan los nombres de los ejes
  theme_bw() 

plot(graf2)

# Se considera que el gráfico adecuado para responder la pregunta es un gráfico de barras, ya que se puede apreciar la cantidad de personas encuestadas en cada provincia de la RM.
# En el gráfico de barras se puede observar una clara diferencia en las alturas de las barras, con una gran altura específicamente en la correspondiente a la provincia "Santiago"
# Por lo tanto se puede concluir del gráfico que no se encuestaron más o menos la misma cantidad de gente en cada provincia de la RM, se encuestarón más personas de la provincia "Santiago"
  