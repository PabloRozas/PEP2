salario= c(11,10,8,5,9,7)
salario = as.integer(salario)

ausencias = c(18,17,29,36,11,28)
ausencias = as.numeric(ausencias)

id = c(1,2,3,4,5,6)
id = as.numeric(id)
datos = data.frame(id,salario,ausencias)
datos

regresion =  lm(ausencias ~ salario, datos)
regresion
print(summary(regresion))

cor.test(datos$salario, datos$ausencias)

plot(datos)
plot(datos$salario, datos$ausencias)
plot(datos$ausencias ~ datos$salario, datos)
abline(52.929, -3.571, col = "blue")



confint(regresion,level = 0.95)























