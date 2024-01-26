dados = sample(x=1:6, size=2, replace=TRUE)
dados

if(dados[1] == dados[2]) {
  print("resultados iguais")
} else if(dados[1] < dados[2]) {
  print("primeiro lançamento foi menor que o segundo")
} else {
  print("segundo lançamento foi menor que o primeiro")
}

iris

# str => structure e não string o.O
str(iris) # obs => linhas; variables => colunas; factor => categorias

iris$Species

iris[,5]

# embaralhar o vetor original de 1..150 e guardar em amostra
amostra = sample(x=1:150, size=150, replace = FALSE)
amostra

# filtrar o conjunto de dados iris usando a amostra como filtro
irisAmostral = iris[amostra,]
irisAmostral

number = round(nrow(iris)*0.8)
number

round(151*0.8)

table(irisAmostral$Species)

treinamento = irisAmostral[1:number,]
treinamento

teste = irisAmostral[(number+1):nrow(irisAmostral),]
teste

treinamento.table = table(treinamento$Species)
treinamento.table
barplot(treinamento.table)

unique(treinamento$Species)


# mostrar todos histogramas (mfrow) = deve ser usado antes dos hist
par(mfrow = c(2,2))

setosa = treinamento[treinamento$
                       Species == "setosa",]
hist(setosa$Petal.Length)

virginica = treinamento[treinamento$Species == "virginica",]
hist(virginica$Petal.Length)

versicolor = treinamento[treinamento$Species == "versicolor",]
hist(versicolor$Petal.Length)


par(mfrow= c(2,2))
# colocar o grafico vazio com todos os dados do treinamento para poder adicionar separadamente setosa/virginica/versicolor
# pois o treinamento contem todas as outras dessa forma todo o conjunto aparecerá no msm grafico
plot(x = treinamento$Petal.Length, y=treinamento$Petal.Width, type="n", pch=16)
points(x=setosa$Petal.Length, y=setosa$Petal.Width, pch=16, col="blue")
points(x=versicolor$Petal.Length, y=versicolor$Petal.Width, pch=16, col="red")
points(x=virginica$Petal.Length, y=virginica$Petal.Width, pch=16, col="green")

plot(x = treinamento$Sepal.Length, y=treinamento$Sepal.Width, type="n", pch=16)
points(x=setosa$Sepal.Length, y=setosa$Sepal.Width, pch=16, col="blue")
points(x=versicolor$Sepal.Length, y=versicolor$Sepal.Width, pch=16, col="red")
points(x=virginica$Sepal.Length, y=virginica$Sepal.Width, pch=16, col="green")

plot(x = treinamento$Sepal.Length, y=treinamento$Petal.Width, type="n", pch=16)
points(x=setosa$Sepal.Length, y=setosa$Petal.Width, pch=16, col="blue")
points(x=versicolor$Sepal.Length, y=versicolor$Petal.Width, pch=16, col="red")
points(x=virginica$Sepal.Length, y=virginica$Petal.Width, pch=16, col="green")

plot(x = treinamento$Petal.Length, y=treinamento$Sepal.Width, type="n", pch=16)
points(x=setosa$Petal.Length, y=setosa$Sepal.Width, pch=16, col="blue")
points(x=versicolor$Petal.Length, y=versicolor$Sepal.Width, pch=16, col="red")
points(x=virginica$Petal.Length, y=virginica$Sepal.Width, pch=16, col="green")

respostas = c()
teste[1,5]

for(j in 1:nrow(teste)) {
  if(teste$Petal.Length[j] < 2) { # pela largura do grafico Petal.width por Petal.length
    respostas[j] = "setosa"
  }else {
    if(teste$Petal.Width[j] < 1.7){ # pela altura do grafico Petal.width por Petal.length
      respostas[j] = "versicolor"
    }else {
      respostas[j] = "virginica"
    }
  }
}
abline(h=1.7)
abline(v=2)
respostas

# comparar com o real
mean(respostas == teste$Species)

