# em média quantas figurinhas são necessárias comprar para preencher o album?
N = 7 # N é o numero de figurinhas do album
numExperimentos = 1e4
figurinhas = 1:N

minhasFigurinhas = sample(figurinhas, replace = TRUE, size=N)

resultados = c()

for(i in 1:numExperimentos) {
	# inicialmente comprei N figurinhas
	minhasFigurinhas = sample(figurinhas, replace = TRUE, size=N)
	while(length(unique(minhasFigurinhas)) < N){
		# comprarei uma figurinha por iteração
		minhasFigurinhas = c(minhasFigurinhas, sample(figurinhas, 1, replace = TRUE))
	}
	resultados = c(resultados, length(minhasFigurinhas))
}

mean(resultados)
# para N = 5 a média é aproximadamente 11.41
# para N = 7 a média é aproximadamente 18.29


# a probabilidade de comprar a 1ª figurinha agora deve ser P e as demais devem ser 3P

# P + 3P + 3P + ... + 3P = 1
# P + 3P*(N-1) = 1

# PONDERADA

N = 5 # N é o numero de figurinhas do album
figurinhas = 1:N
probabilidades = c(1, rep(3, N-1))
resultados = c()

for(i in 1:numExperimentos) {
	# inicialmente comprei N figurinhas
	minhasFigurinhas = sample(figurinhas, replace = TRUE, size=N, prob = probabilidades)
	while(length(unique(minhasFigurinhas)) < N){
		# comprarei uma figurinha por iteração
		minhasFigurinhas = c(minhasFigurinhas, sample(figurinhas, 1, replace = TRUE, prob= probabilidades))
	}
	resultados = c(resultados, length(minhasFigurinhas))
}

mean(resultados)

# para N = 5 e os pesos probabilisticos a média é aproximadamente 11.47
# para N = 7 e os pesos probabilisticos a média é aproximadamente 18.28

