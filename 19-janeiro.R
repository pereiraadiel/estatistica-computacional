dados = sample(x = 1:6, size=10, replace=TRUE);
dados;

lancarDados = function(vezes = 1) {
  dados = sample(x = 1:6, size=vezes, replace=TRUE);
  return(dados);
};

lancarDados(vezes = 5);


sortear = function(from = 1, to=1, vezes=1) {
  sorteado = sample(x=from:to, size=vezes, replace=TRUE)
  return(sorteado)
};

sortear();

# proporção de 3
proporcao3 = function(vezes) {
  sorteado = sample(x = 1:6, size=vezes, replace=TRUE);
  return (
    mean(sorteado == 3)
  );
};

proporcao3(1e6)

# which retornar quais posições de um vetor possui o valor TRUE
which(c(TRUE, FALSE, FALSE, TRUE))

times = 1e1
# cumsum = acumula as vezes que um determinado valor aparece a partir da posição 0 até a posição N do vetor
cumsum(lancarDados(times) == 3)

# dividir por 1:100 = dividir por um vetor de msm tamanho do cumsum para pegar a proporção de 3 ao longo do tempo
proporcao3acumulada = cumsum(lancarDados(times) == 3)/1:times

plot(x = 1:times, y=proporcao3acumulada, type="l")
abline(h= 1/6, col='#ff2233')


# proporcao de caras no lancamento de moedas:
cara = 0
coroa = 1

lancamentos = 1e5

lancarMoeda = function(vezes = 1) {
  dados = sample(x=1:2, size=vezes, replace = TRUE);
  return(dados)
}

proporcaoCaraAcumulada = cumsum(lancarMoeda(lancamentos) == cara)/1:lancamentos

plot(x=1:lancamentos, y=proporcaoCaraAcumulada, type='l')
abline(h =1/2, col="#5566ff")


### ==============
# probabilidade com mega sena

# vetor %in% outroVetor = retorna true nos elementos de vetor que aparecem no outroVetor

variavel = 1
paste("imprimir formatado", variavel)



iris[5,4] # acessar a linha 5 e coluna 4

iris$Sepal.Length # acessar a coluna pelo seu nome (Sepal.Length)


# filtrar somente as que sao da especie "setosa"
filter = iris$Species == 'setosa'
iris$Petal.Width[filter]
setosas = iris[filter,]
