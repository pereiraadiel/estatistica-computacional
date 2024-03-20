mkt = read.table('./marketing.txt', header=TRUE, sep=";")

str(mkt)

boxplot(mkt[, 1:3])
# maior investimento no youtube: 75% do investido no yt ta acima do que foi investido no facebook e quase todos do newspaper

hist(mkt$youtube)
hist(mkt$facebook)
hist(mkt$newspaper)

# modelar vendas em relação ao yt, fb e newspaper	

cor(mkt)

modelo = lm(sales ~ youtube, data=mkt)
vendas = 8.43911 + 0.04754 * mkt$youtube

summary(modelo)

# residuals = erros estatisticos

library(ggplot2)
mkt$previsao = predict(modelo) # add previsao ao dataframe com base na predição do modelo
ggplot(data=mkt, aes(x=youtube, y=sales)) + geom_point() + geom_smooth(method="lm", se=FALSE, col='darkcyan')+geom_segment(aes(x=youtube, xend=youtube, y=sales, yend=previsao), col='darkcyan')+theme_void()


modelo2 = lm(data=mkt[, -5], sales ~ .)
summary(modelo2)
# newspaper nao é significativo em relação a sales (sales em relação a newspaper)


modelo3 = lm(data=mkt[, -5], sales ~ . - newspaper)
summary(modelo3)
# adjusted r-squared quanto mais alto melhor  

