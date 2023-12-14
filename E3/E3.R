library(ggplot2)
library(ggpubr)
library(pwr)

# Se fijan los valores conocidos
mu <- 10
sigma <- 1

# Se calucla el error estandar
se <- sigma/sqrt(100)

set.seed(1000)
datos <- rnorm(100, mu, sigma)

pnorm(c(9.82, 10.25), mean(datos), sd(datos))

# Se grafica la distribucion de la muestra
x <- seq(5, 15, 0.01)
y <- dnorm(x, mean(datos), sd(datos))
g <- ggplot(data = data.frame(x, y), aes(x = x))

g <- g + stat_function(fun = dnorm,
                       args = list(mean = mean(datos), sd = sd(datos)),
                       color = "red",
                       linewidth = 1)

q_critico_inferior <- 9.82
q_critico_superior <- 10.25

g <- g + geom_area(data = subset(data.frame(x, y), x < q_critico_inferior),
                   aes(y = y),
                   color = "red",
                   fill = "red",
                   alpha = 0.5)

print(g)
