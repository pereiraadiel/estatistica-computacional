dados = read.table("./cancer.csv", header=TRUE, sep=",")

head(dados)

str(dados)

unique(dados$diagnosis)

summary(dados)

nrow(dados)

colnames(dados)


# correlação cor
# negativa = nao existe (linear no caso)
# positiva = existe (linear no caso)
# acima de 0.8 é considerado boa correlação

cor(iris$Sepal.Length, iris$Sepal.Width)

ggplot(data= iris, aes(x= Sepal.Length, y = Sepal.Width))+geom_point()


cor(iris$Petal.Length, iris$Petal.Width)

ggplot(data= iris, aes(x= Petal.Length, y = Petal.Width))+geom_point()


cor(iris[,1:4]) # matriz de correlação é simetrica

ggplot(data= iris, aes(x= Petal.Length, y = Petal.Length))+geom_point()

# para uma maior acurácia é melhor usar variaveis que não se correlacionam pois a correlação sao duas ou mais variaveis
# fornecendo o msm dado, o que pode gerar "lixo" no modelo que impactam numa acuràcia menor.


# linear model: tamanho da petala (y) em função da largura da petala (x)
# tamanho da petala = 2.230 * largura + 1.084
lm(iris$Petal.Length ~ iris$Petal.Width)


plot(x= iris$Petal.Width, y= iris$Petal.Length, pch=16, type='p')
min(iris$Petal.Width)
max(iris$Petal.Width)


x = seq(from=0.1, by=0.01, to=2.5)

reta = function(x) {
  return (2.230 * x + 1.084)
}

y = reta(x)

y
lines(x, y, col='red')
