data(iris)

dados = iris[, -5]
dadosPadronizados = scale(dados) # colocar os dados na mesma escala 

matrizDistancia = dist(dadosPadronizados, method = "euclidean")

modelo = hclust(matrizDistancia, method= "ward.D2")
# modelo = hclust(matrizDistancia, method= "single")
plot(modelo)


# destacar os cluster no grafico
rect.hclust(modelo, k=3)

# cortar galhos maiores (mais no topo)
aglomerados = cutree(modelo, k=3)
aglomerados

iris$Species[aglomerados == 1] # imprimir somente as especies do aglomerado 1
