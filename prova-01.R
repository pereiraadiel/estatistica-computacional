#Alunos: Marcus Vinicius Miata 11811BCC017
#        Adiel Pereira 11721BCC008


#Questao 1 - Letra a)

faces <- c("L", "R", "D", "U")
link <- function(N){
  posicao <- c(0, 0)
  for(i in 1:N){
    dado <- sample(x = faces, size = 1, replace=TRUE)
    if(dado == "L"){
      posicao[1] = posicao[1] - 1
    }
    if(dado == "R"){
      posicao[1] = posicao[1] + 1
    }
    if(dado == "U"){
      posicao[2] = posicao[2] + 1
    }
    
    if(dado == "D"){
      posicao[2] = posicao[2] - 1
    }
  }
  return (posicao)
}

resultado <- link(8)

#Letra b

resultados <- c()

for (i in 1:10000 ){
  resultados[i] <- link(8)
}
mean(resultados == c(0,0))


#Letra C

voltaInicio <- function(X){
  if(X %% 2){
    print("Impossível retornar a origem com um numero impar de passos")
    return (0);
  }
  resultados2 <- c()
  for(i in 1:10000){
    resultados2[i] <- link(X)
  }
  return(mean(resultados2 == c(0,0)))
}

voltaInicio(50)


# Questão 2 - A

primatas = read.table("primatas.txt", header=TRUE, sep=":")


summary(primatas)  


# Questao 2 - B

primatas$especie = as.factor(primatas$especie)
primatas$genero = as.factor(primatas$genero)

barplot(table(primatas$especie))
library(ggplot2)

ggplot(data = primatas, aes(x = especie, fill = genero)) + geom_bar()

#Questao C

bonobos <- primatas[primatas$especie == "bonobo", ]
chimpanze <- primatas[primatas$especie == "chimpanze", ]

ggplot(data= bonobos, aes(x = peso, y = altura,  col=genero)) + geom_point()
ggplot(data= chimpanze, aes(x = peso, y = altura,  col=genero)) + geom_point()


#Questão D
femea <- primatas[primatas$genero == "femea", ]
macho <- primatas[primatas$genero == "macho", ]

ggplot(data= femea, aes(x = peso, y = altura,  col=especie)) + geom_point()
ggplot(data= macho, aes(x = peso, y = altura,  col=especie)) + geom_point()

#Questão E
# Na especie de bonobos os machos são um pouco maiores e mais pesados em relação as femeas, já na especie de chimpanzes,
#essa diferença é mais perceptivel, os machos são bem maiores e mais pesados em relação as femeas.
# Agora entre as femeas, as da especie de bonobo são um pouco maiores que as chimpanzes, porém as femeas da especie
#chimpanze são mais pesadas. Agora em relação aos machos, os chimpanzes são tanto maiores quanto mais pesados que os bonobos

#Questão F

amostra <- sample(x = 1:nrow(primatas), size = nrow(primatas), replace = FALSE)

primatas<- primatas[amostra, ]

tamanho <- round(nrow(primatas) * 0.8)
treinamento <- primatas[1:tamanho, ]
teste <- primatas[(tamanho + 1):nrow(primatas), ]

femea <- treinamento[treinamento$genero == "femea", ]
macho <- treinamento[treinamento$genero == "macho", ]

ggplot(data= femea, aes(x = peso, y = altura,  col=especie)) + geom_point(size=1.2, alpha=0.8) + geom_vline(aes(xintercept = 37.5))	# se femea peso < 37.5 = bonobo

ggplot(data= macho, aes(x = peso, y = altura,  col=especie)) + geom_point() + geom_vline(aes(xintercept = 52.5))# se macho e peso < 52.5 = bonobo


respostas <- c()

for(j in 1:nrow(teste)){
  if(teste$especie[j] == "femea"){
    if(teste$peso[j] < 37.5){
      respostas[j] <- "bonobo"
    } else {
      respostas[j] <- "chimpanze"
    }
  } else {
    if(teste$peso[j] < 52.5){
      respostas[j] <- "bonobo"
    } else {
      respostas[j] <- "chimpanze"
    }
  }
}

mean(respostas == teste$especie)
# acuracia de 79%
