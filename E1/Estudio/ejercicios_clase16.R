library(ggplot2)

#Normalizacion de una normal y calcular la probabilidad tomado una muestra

mu = 205
sigma = 48
x_bar = 185

n1 = 25

z = (x_bar - mu)/(sigma/sqrt(n1))
prob = pnorm(z,0,1)

n2 = 5
z2 = (x_bar - mu)/(sigma/sqrt(n2))
prob2 = pnorm(z2, 0,1)

x_prueba = seq(0, 900, length(901))
x = (x_prueba - mu)/(sigma)
y <- dnorm(x,0,1)

plot(x,y, type = 'l', lwd = 2)
rect(xleft = z,0,z,dnorm(z,0,1), col = "blue", lwd = 2)
rect(z2, 0 , z2, dnorm(z2,0,1), col = "green", lwd = 2)