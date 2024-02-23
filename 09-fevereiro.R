titanic = read.table("./titanic.csv", header = T, sep = ",")


str(titanic)


titanic = titanic[, -1] # remover primeira coluna

titanic$Survived = as.factor(titanic$Survived)


library(ggplot2)

ggplot(data = titanic, aes(x=Survived))+geom_bar()+facet_wrap(~Sex)

titanic$Pclass = as.factor(titanic$Pclass)

ggplot(data = titanic, aes(x=Pclass))+geom_bar()

ggplot(data = titanic, aes(x=Pclass, fill=Survived))+geom_bar() # mortes por classe

ggplot(data = titanic, aes(x=Pclass, fill=Survived))+geom_bar()+facet_wrap(~Sex) # mortes por classe e sexo


titanic$Age
sum(is.na(titanic$Age))

# proporçao de dados faltantes de idade
sum(is.na(titanic$Age)) / nrow(titanic)

# usar nomes para "inferir" idades faltantes

ggplot(data = titanic, aes(x=Age, fill=Survived))+geom_histogram()+facet_wrap(~Sex)+labs(title="Histograma para idade", x='Idade', y='Frequência') # mortes por idade, fatiado em funçao do sexo



## =======

cancer = read.table("./cancer.csv", header = T, sep = ",")

str(cancer)
summary(cancer)
cancer$diagnosis = as.factor(cancer$diagnosis)

numObs = nrow(cancer)
amostra = sample(1:numObs, size=numObs, replace=FALSE)

# dados de cancer embaralhados
cancerAmostral = cancer[amostra,]

amostraTreino = round(numObs * 0.8)

# conjunto de treino dos dados de cancer
treino = cancerAmostral[1: amostraTreino,]

# conjunto de teste dos dados de cancer
teste = cancerAmostral[(amostraTreino + 1): numObs,]


ggplot(data = treino, aes(x=diagnosis))+geom_bar()

ggplot(data = treino, aes(x=radius_mean))+geom_histogram()+facet_wrap(~diagnosis)

ggplot(data = treino, aes(y=radius_mean))+geom_boxplot()+facet_wrap(~diagnosis)

# smoothness_mean x radius_mean em função do raio medio
ggplot(data = treino, aes(x=smoothness_mean, y=radius_mean, color=diagnosis, size=1.15, alpha=0.8))+geom_point()

# texture_mean x radius_mean em função do raio medio
ggplot(data = treino, aes(x=texture_mean, y=radius_mean, color=diagnosis, size=1.15, alpha=0.8))+geom_point()

# texture_mean x concavity_mean em função do raio medio
ggplot(data = treino, aes(x=texture_mean, y=concavity_mean, color=diagnosis, size=1.15, alpha=0.8))+geom_point()


cor(treino[, -1])

library(class)
?knn

# variaveis explicativas
treino.x = scale(treino[,-1]) # tirar coluna de diagnostico e colocar dados na msm escala
teste.x = teste[, -1] 
treino.y = scale(treino[,1]) # tirar coluna de diagnostico
teste.y = teste[, 1] 


# cl = nao pode ter variavies nao numericas por isso precisamos de treino.x treino.y, etc
modelo.knn.cancer = knn(train = treino.x, test = teste.x, cl= treino.y, k=3)

mean(modelo.knn.cancer == teste$diagnosis)

table(modelo.knn.cancer,teste$diagnosis)
