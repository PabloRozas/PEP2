library(ggplot2)

arriendo = c(10,12,14,15,15,19,13,17,18,16)
n = length(arriendo)

media_muestral = mean(arriendo)
S_muestral = sd(arriendo)

# Si se quiere llegar a la media x
set.seed(123)
n_boot = 30000
bs = replicate(n = n_boot, sample(arriendo,n,replace = T))
dist_madia_bs = colMeans(bs)

g = ggplot() +
  geom_histogram(aes(x = dist_madia_bs), bins = 30, fill ="darkcyan", color = "black") +
  geom_vline(xintercept = mean(dist_madia_bs), size = 1, color = "darkred") +
  xlab("Arriendo de automoviles $Miles") + ylab("Frecuencia") +
  theme_bw()

plot(g)

media_bs = mean(dist_madia_bs)

sd_bs = sd(dist_madia_bs)