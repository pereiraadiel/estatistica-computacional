data(iris)

dados = iris[sample(nrow(iris)), ]

amostra = round(nrow(dados)*0.8)

treino = dados[1:amostra, ]
teste = dados[(amostra + 1):nrow(dados), ]

nrow(teste)

passoIntervalo = amostra/10
intervalos = seq(from= 1, to=amostra, by=passoIntervalo)

intervalos = c(intervalos, amostra) # dividir o conjunto de treino em 10 partes para validação cruzada

acertos = rep(0, times=10)

# usar subintervalos para teste e treino
# isto é, a partir do teste, usar subintervalos(regiões) para treino
# ou seja CROSS VALIDATION
for (i in 1:(length(intervalos) - 1)) {
	indices = intervalos[i]:(intervalos[i+1]-1)

	testeParcial = treino[indices, ]
	treinoParcial = treino[-indices, ]
	acertosParcial = c()
	for(k in 1:10) { # testar vizinhos de 1 a 10 para determinar qual o melhor k na região atual (i)
		modelo.knn = knn(train = treinoParcial[,-5], test= testeParcial[,-5], cl=treinoParcial$Species, k=k)	
		acertosParcial[k] = mean(modelo.knn == testeParcial$Species); # acertos para cada k na região atual (i)
	}
	acertos = acertos + acertosParcial # soma dos acertos de cada k em todas as regiões
}

mediaAcertos = acertos / 10

plot(x = 1:10, y=mediaAcertos, type='l') # pico = k ideal
kIdeal = which.max(acertos);

modeloFinal = knn(train=treino[,-5], test=teste[,-5], cl=treino$Species, k=kIdeal)

mean(modeloFinal == teste$Species)

