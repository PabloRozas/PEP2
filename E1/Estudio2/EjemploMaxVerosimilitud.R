datos = read.csv("iris.data", sep = ",", header = F)

frec_abs = as.numeric(table(datos$V5))
frec_acum = cumsum(frec_abs)
frec_rel = as.numeric(frec_abs/sum(frec_abs))*100
frec_rel_acum = cumsum(frec_rel)
iris = c("Setosa", "Versicolor", "Virginica")

frec_iris = data.frame(iris, frec_abs, frec_acum, frec_rel, frec_rel_acum)
knitr::kable(frec_iris)


# Tabla de frecuncia
#longitud del sepal
library(ggplot2)


g1 = ggplot(datos, aes(x = datos$V1)) +
  geom_histogram() +
  theme_bw()
plot(g1)


