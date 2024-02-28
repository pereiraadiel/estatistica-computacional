# Lista 2 - Estatistica Computacional
# Marcus Vinicius Miata - 11811BCC017
# Adiel Pereira - 11721BCC008

library(ggplot2)

#Exercicio 1
#Considere o seguinte jogo: Steven e Garnit escolherão, cada um, uma sequência de tamanho 3 em que cada entrada da 
#sequência é cara ou coroa; logo em seguida, uma moeda será lançada três vezes; seaparecer a sequência de um dos 
#jogadores, este jogador vence e o jogo acaba; caso não apareça a sequência denenhum deles, 
#a moeda é lançada pela quarta vez e os três últimos lançamentos são analisados; se nestes três últimos lançamentos 
#aparecer a sequência de um dos jogadores, este jogador vence e o jogo acaba. Se istonão acontecer, a moeda é l
#ançada pela quinta vez e os três últimos resultados são analisados; se aparecer a sequência de um dos jogadores, 
#este jogador vence e o jogo acaba. Este processo é realizado até que apareça a sequência que um dos dois escolheu; 
#se aparecer primeiro a sequência de Steven, ele ganha; se aparecer primeiro a sequência de Garnit, ela vence. 
#Convencione que cara seja 1 e que coroa seja zero. Supondo que Steven escolheu a sequência (0, 1, 0) e que Garnit 
#escolheu a sequência (0, 0, 1), simule uma partida deste jogo. A simulação deve retornar steven caso Steven tenha 
#vencido ou deve retornargarnitcaso contrário.Replique o experimento 10 mil vezes e calcule a média de vitórias de Garnit. 
#Comente o resultado obtido.

jogo <- function(){
  steven <- c(0,1,0)
  garnit <- c(0,0,1)
  
  moedas <- sample(x = c(0,1), size = 3, replace = TRUE)
  
  if(mean(moedas == steven) == 1){
    return ("Steven")
  } 
  if(mean(moedas == garnit) == 1){
    return ("Garnit")
  }  
  
  while(1){  
    nova <- sample(x = c(0,1), size = 1, replace = TRUE)
    moedas <- moedas[2:3]
    moedas <- c(moedas, nova)
    if(mean(moedas == steven) == 1){
      return ("Steven")
    } 
    if(mean(moedas == garnit) == 1){
      return ("Garnit")
    }
  }
}

resultados <- c()
for (i in 1:10000){
  resultados[i] <- jogo()
}

mean(resultados == "Garnit")

#Ao se fazer o experimento 10.000 vezes se percebe que a chance de vitória do Garnit é de aproximadamente 66%, ou seja,
#a seguencia inicial escolhida por ele é melhor que a seguencia escolhida por steven

#Exercicio 2
dados <- read.table(file = "dados.txt", header = TRUE, sep = ";")

dados$LocalDaMorte <- as.factor(dados$LocalDaMorte)
dados$Genero <- as.factor(dados$Genero)

#A)
summary(dados)
ggplot(data = dados, mapping = aes(x = Genero, fill = Genero)) + geom_bar() + theme_minimal()
#Percebe-se que o médico tinha como vitimas principais mulheres

#B)
ggplot(data = dados, mapping = aes(x = Idade, fill = Genero)) + geom_histogram(bins = 8) +
  scale_x_continuous(breaks = seq(41, 100, by = 8))
#Dá pra ver que além da preferencia por mulheres, o assassino tinha uma preferencia por pessoas entre 70 e 85 anos

#C)
ggplot(data = dados, mapping = aes(y = Idade, x = Genero)) + geom_boxplot()
#Percebe-se que na media as vitimas do sexo masculino eram mais velhas que as vitimas do sexo feminino

#D)
ggplot(data = dados, mapping = aes(x = LocalDaMorte, fill = Genero)) + geom_bar() + theme_minimal()
#O principal local que ele matava as pessoas era em suas proprias casas

#E)
ggplot(data = dados, mapping = aes(x = AnoDaMorte, fill = Genero)) + geom_histogram()
# Ele esteve mais ativo entre 1993 - 1998

#F) Com Base em todos estes gráficos que foram apresentados, pode-se dizer que o perfil de vitimas do médico
#eram mulheres entre 70 e 85 anos, entre 1993-1998 foram os anos em que ele mais executou mulheres e seu plano
#de ação normalmente era cometer os assassinatos na casa da vitima

#Exercicio 4

cogumelos <- read.csv("cogumelos.csv")
cogumelos <- data.frame(lapply(cogumelos, as.factor))

summary(cogumelos)

amostra <- sample(x = 1:nrow(cogumelos), size = nrow(cogumelos), replace = FALSE)
cogumelos <- cogumelos[amostra, ]

tamanho <- round(nrow(cogumelos) * 0.80) 
treinamento <- cogumelos[1:tamanho, ]
teste <- cogumelos[(tamanho+1):nrow(cogumelos), ]

ggplot(data = treinamento, mapping = aes(x = odor, fill = class)) + geom_bar()
#a,l certeza de ser e
#c, f,m, p,s,y certeza de ser p 
#n grandes chances de ser e

respostas <- c()

for(j in 1:nrow(teste)){
  if(teste$odor[j] == "a" || teste$odor[j] == "l" || teste$odor[j] == "n"){
    respostas[j] <- "e"
  }else{
    respostas[j] <- "p"
  }
}

mean(respostas == teste$class)

#A taxa de acerto assim fica bem alta, porém existe um risco (baixo) de ser envenenado caso o cheiro seja n, que é onde está o erro do modelo

# Exercício 3. Os arquivos treino_baleias.txt e teste_baleias.txt contém informações sobre as carac-
# terísticas de algumas espécies de baleias. Os conjuntos de dados possuem, ao todo, 248 observações (198
# para treino, 50 para teste). As variáveis incluídas nestes conjuntos de dados são:
# 1• especie: indica a espécie da baleia e é uma variável categórica;
# • comprimento: indica o comprimento da baleia em metros e é uma variável numérica contínua;
# • peso: indica o peso da baleia em quilos e é uma variável numérica contínua;
# • profundidade_maxima: indica a profundidade máxima mergulhada pela baleia em metros e é uma
# variável numérica contínua;
# • volume_cranio: indica o volume do crânio da baleia em centímetros cúbicos e é uma variável numérica
# contínua.
# Os itens de (a) até (e) devem ser respondidos usando apenas os dados de treino_baleias.txt. Em (f), os
# dois conjuntos devem ser utilizados.

# a)Crie um conjunto para cada espécie de baleia; cada data frame criado deverá conter apenas baleias de
# uma espécie.

baleias_treino <- read.table(file = "treino_baleias.txt", header = TRUE, sep = ",")
baleias_treino$especie <- as.factor(baleias_treino$especie)

# b) Calcule a média, a variância, o desvio padrão e o coeficiente de variação para a variável peso para cada
# espécie de baleia. Comente os resultados obtidos. (usando mean)

medias <- aggregate(peso ~ especie, data = baleias_treino, FUN = mean)
variancias <- aggregate(peso ~ especie, data = baleias_treino, FUN = var)
desvios <- aggregate(peso ~ especie, data = baleias_treino, FUN = sd)
coeficientes <- desvios$peso / medias$peso

# c)Apresente o histograma da variável peso para a espécie de baleia azul. Comente os resultados obtidos.

baleias_azuis <- baleias_treino[baleias_treino$especie == "azul",]

# d) Apresente numa mesma janela os boxplots para cada espécie para a variável comprimento. Comente
# os resultados obtidos.

ggplot(data = baleias_treino, mapping = aes(x = especie, y = comprimento, fill = especie)) + geom_boxplot()

# pelo grafico boxplot concluimos que a especie azul é a que tem o maior comprimento,
# a especie jubarte é a que tem o menor comprimento.
# alem disso podemos notar que 75% das baleias azuis tem um comprimento maior que as das demais especies

# e) Apresente um gráfico de dispersão de comprimento versus profundidade_maxima. Cada espécie deve
# ser registrada por uma cor diferente.

ggplot(data = baleias_treino, mapping = aes(x = comprimento, y = profundidade_maxima, color = especie)) + geom_point()

# pelo grafico notamos que quanto maior a baleia mais profundo ela mergulha

# f) Com base em todas as informações anteriores, construa um modelo de árvore de decisão a partir de
# estruturas condicionais e de repetição para prever a espécie de uma baleia com base nas variáveis
# numéricas do estudo. Justifique as escolhas das variáveis e dos pontos de corte escolhidos. Por fim,
# utilize o conjunto do arquivo teste_baleias.txt para calcular a taxa de acerto. Comente o resultado
# obtido.

baleias_teste <- read.table(file = "teste_baleias.txt", header = TRUE, sep = ",")
baleias_teste$especie <- as.factor(baleias_teste$especie)

# A variavel escolhida foi o comprimento, pois como vimos no boxplot, as especies variam consideravelmente em comprimento

respostas <- c()

# baleias: azul, jubarte, fin, cachalote

for(j in 1:nrow(baleias_teste)){
  if(baleias_teste$comprimento[j] > 28){
    respostas[j] <- "azul"
  }else if(baleias_teste$comprimento[j] > 22){
    respostas[j] <- "fin"
  }else if(baleias_teste$comprimento[j] > 19){
    respostas[j] <- "cachalote"
  }else {
    respostas[j] <- "jubarte"
  }
}

mean(respostas == baleias_teste$especie)

# A taxa de acerto foi de 0.92, ou seja, 92% das baleias foram classificadas corretamente

# g) Utilize gráficos de dispersão para registrar por linhas horizontais e verticais os pontos de cortes escolhidos
# em sua árvore de decisão. As espécies de baleias devem ser registradas por diferentes cores.

ggplot(data = baleias_treino, mapping = aes(x = comprimento, y = profundidade_maxima, color = especie)) + geom_point() +
  geom_vline(xintercept = 28, color = "black") + geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 22, color = "black") + geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 19, color = "black") + geom_hline(yintercept = 0, color = "black")

# h) Crie um modelo de classificação KNN para classificar as baleias. Utilize 𝐾 = 1 e depois 𝐾 = 3.
# Compare os resultados dos dois modelos KNN.

library(class)

knn1 <- knn(train = baleias_treino[,2:5], test = baleias_teste[,2:5], cl = baleias_treino$especie, k = 1)
knn3 <- knn(train = baleias_treino[,2:5], test = baleias_teste[,2:5], cl = baleias_treino$especie, k = 3)

mean(knn1 == baleias_teste$especie)
mean(knn3 == baleias_teste$especie)

# A taxa de acerto do knn com k = 1 foi de 0.98, ou seja, 98% das baleias foram classificadas corretamente
# A taxa de acerto do knn com k = 3 foi de 0.98, ou seja, 98% das baleias foram classificadas corretamente