install.packages("factoextra")

library(rvest)
library(dplyr)
library(ggplot2)
library(factoextra)


dados = read.table("protein.txt", header=TRUE, sep="\t")
dadosPadronizados = scale(dados[, -1])

matrizDistancias = dist(dadosPadronizados, method = "euclidean")

modelo = hclust(matrizDistancias, method = "ward.D2")

plot(modelo)
fviz_dend(modelo, k=5)
aglomerados = cutree(modelo, k=5)

dados$Country[aglomerados == 1]

summary(dados[aglomerados == 4, ])
