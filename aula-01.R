fibo = c(1, 1, 2, 3, 5, 8, 13)
fibo[c(1, 6)] # imprimir o 1º e o 6º termo de fibo


seq = c(1: 100)
seq


sample(5) # 5 numeros "aleatorios"
set.seed(1234) # definir semente para obter os mesmos valores "aleatorios"
runif(10) # obter 10 numeros "aleatorios" baseados no seed acima

random = sample(5)
filter = random < 3 # atribui true para as posicoes que forem menores que 3

filteredRandom = random[filter]
filteredRandom
random
