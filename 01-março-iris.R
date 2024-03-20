library(ggplot2)
library(GGally)

data(iris)

ggcorr(iris[,-5])
ggcorr(iris[,-5], label=TRUE)

library(ISLR2)

boston = Boston
str(boston)


# mapa de calor (correlação entre as variaveis do conjunto)
ggcorr(boston, label=TRUE, label_round=2, size=2)


# attach(dados) para nao precisar passar data = boston, porém aplica pra todo o script
modelo = lm(medv ~lstat, data=boston)
summary(modelo)

ggplot(data = boston, aes(x=medv, y=lstat))+geom_point()+geom_smooth(method="lm", se=TRUE, color="darkcyan", lwd=2)


hist(modelo$residuals)

modelo2 = lm(data= boston, medv ~ lstat + rm) # nao soma lstat e rm apenas adiciona as duas no modelo
summary(modelo2)


hist(modelo2$residuals)


modelo3 = lm(data =boston, medv ~ .)
summary(modelo3)

hist(modelo3$residuals)


modelo4 = lm(data =boston, medv ~ . - age - indus)
summary(modelo4)

hist(modelo4$residuals)


predict(modelo4, interval='confidence')
