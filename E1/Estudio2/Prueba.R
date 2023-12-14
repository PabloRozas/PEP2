library(ggplot2)
library(ggpubr)
library(boot)

datos <- read.csv("iris.data",  sep = ",", header = FALSE)

g1  = ggplot(datos, aes( x = V5, y = V1)) +
  geom_boxplot() +
  theme_bw() 


g2 = ggplot(datos, aes(x = V5, y = V1)) +
  geom_boxplot() +
  theme_bw()


ggarrange(g1, g2, ncol = 2)

datos_setosa = datos[datos$V5 == "Iris-setosa",]

g3  = ggplot(datos_setosa, aes(x = V1)) +
  geom_density() +
  theme_bw()
plot(g3)

g4 =  ggplot(datos[datos$V5 == "Iris-versicolor",], aes(x = V1)) +
  geom_density() + 
  theme_bw()

g5 =  ggplot(datos[datos$V5 == "Iris-virginica",], aes(x = V1)) +
  geom_density() + 
  theme_bw()

ggarrange(g3, g4, g5, ncol = 3)

datos_muestrales <- head(datos, n = 10)

n_boot = 15000

bs = replicate(n = n_boot, sample(datos_muestrales$V1,n_boot,replace = T))
media_bs = colMeans(bs)
media_muestral = mean(media_bs)

g6 = ggplot() +
  geom_histogram(aes(x = media_bs), bins = 60, fill = "darkcyan", color = "black") + 
  geom_vline(xintercept = mean(media_bs), size = 1, color = "darkred") +
  theme_bw()

plot(g6)
 
media_poblacional_setosa = mean(datos[datos$V5 == "Iris-setosa",]$V1)
 
