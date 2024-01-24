x = c(1, 65, 32, 45)
x = c(x, 10)

# estudar listas no R

x[c(2, 3)] / 2


y = c(2, 4, 5, 6, 1)

# o R recicla os vetores menores, isto é, se uma operação for executada entre vetores de tamanhos distintos o menor vetor será
# reutilizado até atingir o tamanho do vetor maior: ex: [1, 2] +  [3, 4, 5] = [1+ 3 =4, 2 + 4 = 6,1 + 5 = 6]

# operando vetores por vetores
x / y
x * y
x + y
x - y
x ** y


# ?{nome-funcao} abrir ajuda para a função nome-funcao

s = seq(1, 10, by = 2)
s

seq(10, 1, -2)

seq(1, 100, length.out = 50)


# simular lancamento de dado
lancamentos = sample(1:6, size=1e8, replace=TRUE)
sample(x, 3)


# media
mean(lancamentos)

# calcular qnts vezes o 4 apareceu nos experimentos
mean(lancamentos == 4)



# lançando duas vezes um dado qual a probabilidade de a soma ser menor igual a 4
  num_experiments = 1e8
  one = sample(1:6, size=num_experiments, replace=TRUE)
  two = sample(1:6, size=num_experiments, replace=TRUE)
  
  soma = one + two
  soma
  
  mean(soma <= 4)
  mean(soma <= 3)
  
  
# calcular a media de 500 lancamentos do experimento de lancamentos de dois dados acima
medias = c()
num_experiments = 1e5

for (i in 1:50) {
  one = sample(1:6, size=num_experiments, replace=TRUE)
  two = sample(1:6, size=num_experiments, replace=TRUE)
  
  soma = one + two
  
  media = mean(soma <= 4)
  medias = c(medias, media)
  # medias[i] = media
}

hist(medias)
