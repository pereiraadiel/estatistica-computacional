n = as.integer(1e6)
resultados = sample(1:6, size=n, replace=T)

mean(resultados)

somaAcumulada = cumsum(resultados)/(1:n)

plot(x=1:n, y=somaAcumulada, type="l")
abline(h=3.5, col="red")
x = c(3, 4, 19)
cumsum(x)

# estimar a probabildade da soma ser 7 no lancamento de dois dados
sum(sample(1:6, size=2, replace=T))
x = c()

for(k in 1:as.integer(1e6)) {
  x[k] = sum(sample(1:6, size=2, replace=T)) == 7
}

x[1:10]
mean(x)


# experimentos sobre aniversarios no msm dia
n = 15 # num pessoas

# estimar a probabilidade das 2 pessoas em N pessoas fazerem aniversario no msm dia
x = sample(1:365, size = n, replace=T)
length(unique(x))

resultado = length(unique(x)) < n # verdadeiro se pelo menos 2 pessoas fazem aniversario no msm dia
resultado

# monte carlo: repetir o experimento varias vezes
resultados = c()
n = 15

for(k in 1:1e5) {
  x = sample(1:365, size = n, replace=T)
   resultados[k] <- length(unique(x)) < n # pelo menos 2 no msm dia
  # resultados[k] <- length(unique(x)) < n -1 # pelo menos 3 no msm dia
}

mean(resultados)


# estimar o PI usando um circulo inscrito sobre um quadrado de lado = 2
pointX = runif(1e4, -1 , 1)
pointX

pointY = runif(1e4, -1, 1)
pointY
ordem = 1:1e3


c(pointX, pointY)

isPointInCirc = pointX^2 + pointY^2 <= 1
isPointInCirc

PI = mean(isPointInCirc) * 4 # estimativa do PI

dados = data.frame(pointX, pointY, isPointInCirc)

install.packages(gganimate)
library(ggplot2)
library(gganimate)
ggplot(dados, aes(pointX, pointY, col=isPointInCirc))+geom_point()+coord_fixed()+transition_states(ordem)+shadow_mark(past=T)
