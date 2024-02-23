#Adiel Pereira Prado -  11721BCC008
#Marcus Vinicius Miata - 11811BCC017

# Exercício 1. Crie os seguintes vetores:
#(a) (10, 11, 12,…, 30)
#(b) (30, 29, 28,…, 10)
#(c) (10, 11, 12,…, 30, 29, 28,…, 10)
vetor1A = c(10:30)
vetor1B = c(30:10)
vetor1C = c(vetor1A, vetor1B)

#Exercício 2. Use a função help do R para descobrir o funcionamento das funções rep e seq. Em seguida,
#utilize estas funções para resolver os seguintes itens:
#(a) Crie o vetor (2, 4, 6, 8, 2, 4, 6, 8,…, 2, 4, 6, 8), em que há dez ocorrências do número 2.
#(b) Crie o vetor (2, 4, 6, 8, 2, 4, 6, 8,…, 2, 4, 6, 8, 2), em que há onze ocorrências do número 2 e dez
#ocorrências dos números 4, 6 e 8.
#(c) Considere o vetor (3, 7, 1). Crie um novo vetor que repita os valores do vetor original três vezes.
#Depois, crie outro vetor onde o primeiro valor do vetor original se repita 4 vezes, o segundo valor se
#repita duas vezes e o terceiro se repita três vezes mantendo a ordem original.

vetorBaseA = c(2, 4, 6, 8)
vetor2A = rep(vetorBaseA, 10)
vetor2B = c(vetor2A, 2)

vetorBaseB = c(3, 7, 1)
vetor2C = rep(vetorBaseB, 3)
vetor2C1 = c(rep(vetorBaseB[1], 4), rep(vetorBaseB[2], 2), rep(vetorBaseB[3], 3))

#Exercício 3. Utilize a estrutura de vetores do R para realizar as seguintes somas:
#30
#(a) ∑ (𝑛2 + 4𝑛);
#𝑛=20
#20
#𝑛
#𝑛
#(b) ∑ ( 3𝑛 + 2𝑛2 ).

somatorioA = c()
for(i in 20:30){
	somaA = i^2 + 4*i
	somatorioA = c(somatorioA, somaA)
}
sum(somatorioA)

somatorioB = c()
for(i in 20:10){
	somaB = 3*i + 2*i^2
	somatorioB = c(somatorioB, somaB)
}
sum(somatorioB)

#Exercício 4. Numa urna há bolas idênticas numeradas de 1 até 100. Serão extraídas 40 bolas com reposição
#desta urna. Simule este experimento e guarde o resultado dos sorteios em um vetor.
#(a) Quantas bolas pares foram sorteadas?
#(b) Quantas bolas maiores do que 70 foram sorteadas?
#(c) Em quais retiradas (posições) foram sorteadas as bolas ímpares?

sorteio = c()
for(i in 1:40){
	sorteio = c(sorteio, sample(1:100, 1))
}

pares = sorteio[sorteio %% 2 == 0]
length(pares)

maiores70 = sorteio[sorteio > 70]
length(maiores70)

impares = sorteio[sorteio %% 2 != 0]
match(impares, sorteio)

#Exercício 5. Crie um função no R que irá simular sucessivos lançamentos de um dado até que o número 4
#seja obtido pela segunda vez. A função deverá retornar o número de lançamentos que foram necessários até
#o 4 ser obtido pela segunda vez. Assim, se os sorteios foram 3, 6, 6, 5, 4, 2, 4 a função deverá retornar 7.
lancarDados = function(){
	lancamento = sample(1:6, 1)
	if(lancamento == 4){
		return(TRUE)
	}
	return(FALSE)
}


lancamentosAteSegundo4 = function() {
  lancamentos = c()
  contador = 0
  while (TRUE) {
    if (length(lancamentos[lancamentos == TRUE]) >= 2) {
      break
    }
    lancamento = lancarDados()
    lancamentos = c(lancamentos, lancamento)
    contador = contador + 1
  }
  return(contador)
}

lancamentosAteSegundo4()


#Exercício 6. Utilize a função do exercício anterior para replicar o experimento dez mil vezes. Para cada
#replicação, guarde o número de lançamentos num vetor chamado quantidades. Por fim, calcule a média de
#quantidades. Interprete o resultado obtido.

quantidades = c()
iteracoes = 1e4 # 10.000
for(i in 1:iteracoes){
	quantidades = c(quantidades, lancamentosAteSegundo4())
}
mean(quantidades)
# Interpretação
# Em média são necessários 11.98 lançamentos para que o número 4 seja obtido pela segunda vez.

#Exercício 7. Os dois primeiros termos da sequência de Fibonacci são iguais a 1. Os termos subsequentes
#da sequência são encontrados somando os dois termos imediatamente anteriores. Escreva uma função com
#parâmetro de entrada n chamada fibonacci que retornará os primeiros n termos da sequência de Fibonacci
#para qualquer 𝑛 ≥ 3

fibonacci = function(n){
	fibona = c(1, 1)
	for(i in 3:n){
		fibona = c(fibona, fibona[i-1] + fibona[i-2])
	}
	return(fibona)
}

fibonacci(15)
  
#Exercicio 8. Michael Scott é gerente regional da empresa Dunder Mufflin. Para as festividades de fim de ano, 
#Michael propôs aos funcionários Dwight Schrute, Jim Halpert, Kevin Malone e Creed Bratton a realização de 
#um amigo oculto entre eles. Consideraremos que o sorteio do amigo oculto deu errado quando uma pessoa sortear 
#ela mesma (Michael tira Michael, por exemplo). Simule o sorteio do amigo oculto. Se ele deu certo, 
#atribua o valor 1; caso contrário, atribua o valor 0 (zero). Em seguida, replique este experimento cem mil 
#vezes e calcule a proporção de vezes que o amigo oculto deu errado.

participantes <- c("Dwight Schrute", "Jim Halpert", "Kevin Malone", "Creed Bratton")

resultados <- c()

for (i in 1:100000){
  sorteio <- sample(x = participantes, size = 4, replace = FALSE) 
  if(sum(participantes == sorteio) > 0){ #Considerando que cada participante pegue o nome na ordem do vetor inicial
    resultados[i] <- 0
  } else{
    resultados[i] <- 1
  }
}

media <- mean(resultados == 0) * 100
cat("O sorteio do amigo secreto deu errado em ", media, "% vezes")


#Exercicio 9. No jogo de Craps dois dados são lançados:
# se a soma for 7 ou 11, você ganha o jogo;
#•se a soma for 2,3 ou 12, você perde o jogo;
#•caso contrário, os dois dados são rolados novamente até obter-se 7 (você perde) ou 
#até obter-se a soma inicial (você ganha).

resultados <- c()

for (i in 1:100000){
  dados <- sample(x = 1:6, size = 2, replace = TRUE)
  soma <- sum(dados)
  
  if(soma == 7 || soma == 11){
    resultados[i] <- 1
  } else {
    if(soma == 2 || soma == 3 || soma == 12){
      resultados[i] <- 0
    } else{
      inicial <- soma
      dados <- sample(x = 1:6, size = 2, replace = TRUE)
      soma <- sum(dados)
      while(TRUE){
        if(soma == inicial){
          resultados[i] <- 1
          break
        } 
        if(soma == 7){
          resultados[i] <- 0
          break
        }
        dados <- sample(x = 1:6, size = 2, replace = TRUE)
        soma <- sum(dados)
      }
    }
  }
}

media <- mean(resultados == 1) * 100
cat("Você venceu: ", media, "% das vezes que jogou.")


#Exercicio 10. Luke Skywalker realizará o seguinte passeio aleatório na reta: a reta do passeio é formada
#pelos números inteiros de zero até𝑁; Luke está em um ponto𝐿que é maior do que zero e menor do que𝑁;
#Luke lança uma moeda honesta; se sair coroa, ele dá um passo para a esquerda (e termina na posição𝐿 − 1da reta); 
#se sair cara, ele dá um passo para a direita (e termina na posição𝐿 + 1da reta). Luke continuaráa lançar a moeda
#e se deslocará até que ele chegue em sua casa (e lá ele vai dormir e o passeio acaba) ou atéque ele chegue (caia)
#no precipício (e, óbvio, o passeio também acaba nesse caso).

# a) Para𝑁 = 20, crie uma função cuja entrada seja𝐿(um número maior do que zero e menor do que 20)e 
#que retorne 1 se Luke terminou um passeio em sua casa ou retorne zero se Luke caiu no precipício.

passeio <- function(L){
  
  while(TRUE){
    sorteio <- sample(x = c("cara", "coroa"), size = 1, replace = TRUE)
    if(sorteio == "coroa"){
      L <- L - 1
    } else {
      L <- L + 1
    }
    if(L == 0){
      return(0)
    }
    if(L == 20){
      return(1)
    }
  }
}

#b) Crie uma função cuja entrada seja𝐿; esta função deverá replicar o passeio da letra (a) 
#10 mil vezes eretornar a proporção de vezes que Luke chegou em sua casa. Sugestão: crie um vetor que, 
#para cadareplicação, guardará o resultado de um passeio; cada entrada deste vetor será zero ou 1; zero se Luke
#caiu no precipício e 1 se Luke chegou em casa.

passeio10k <- function(L){
  resultados <- c()
  for(i in 1:10000){
    resultados[i] <- passeio(L)
  }
  return(mean(resultados == 1))
}

passeio10k(10)

#c) Use a função criada em (b) para𝐿 = 1, 2, ... , 19e, em seguida, use esses valores para plotar um gráfico 
#de𝑥 = 1 ∶ 19 por𝑦, em que𝑦são as proporções retornadas pela função criada em (b) para cada𝑥

medias <- c()
for (j in 1:19){
  medias[j] <- passeio10k(j)
}

plot(x = 1:19, y = medias, type = 'o', pch = 16, xlim = c(1, 19),ylim = c(0, 1), axes = TRUE, col = "blue")
axis(1, at = seq(1, 19, by = 1))
axis(2, at = seq(0, 1, by = 0.1))
