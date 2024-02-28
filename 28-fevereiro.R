install.packages('rvest')
install.packages('tidyverse')
library(rvest)
library(tidyverse)
library(stringr)
# library(httr) # raspagem de dados de apis


url = "https://mathbits.com/MathBits/TISection/Statistics2/linearREAL.html"

html = read_html(url)

tabela = html |> html_elements("table.blackbordergreen") |> html_table()
str(tabela)

tabela = tabela[[1]]

dataframe = as.data.frame(tabela) 

dataframe = (dataframe[-1,])

colnames(tabela) = c("Frequência de som", "temperatura ºF") # renomear colunas das tabelas

dataframe$`Frequência de som` = as.numeric(dataframe$`Frequência de som`)
dataframe$`temperatura ºF` = as.numeric(dataframe$`temperatura ºF`)

# converter de fareheit para celsius
# formula 5/9 * (F - 32)
dataframe$`temperatura ºC` = 5/9 * (dataframe$`temperatura ºF` - 32)


library(ggplot2)
ggplot(data= dataframe, aes(y=`Frequência de som`, x=`temperatura ºC`)) + geom_point()

correlacao = function (x, y) {
	numerador = sum(x*y) - length(x)*mean(x)*mean(y)
	denominador = sqrt(sum(x^2) - length(x)*mean(x)^2) * sqrt(sum(y^2) - length(y)*mean(y)^2)
	coeficiente = numerador/denominador
	return(paste0("O coeficiente de relação entre estes dois vetores é: ", coeficiente))
}

# correlação alta entre mais de uma variavel é ruim pois significa que ambas as variaveis dizem a mesma coisa no conjunto
# porem para relação 1:1 é bom

correlacao(dataframe$`Frequência de som`, dataframe$`temperatura ºC`)
cor(dataframe)


# qual a melhor reta para descrever Frequecia de som em função da temperatura (y em relaçao a x)

modelo = lm(`Frequência de som` ~ `temperatura ºC`, data = dataframe)
# com isso temos que o som é 0.3654 * temperatura + 6.9553
# som = 0.3654 * temperatura + 6.9553 (por segundo)

# plotar o modelo no grafico
ggplot(data= dataframe, aes(y=`Frequência de som`, x=`temperatura ºC`)) + geom_point() + geom_smooth(method = "lm", se=TRUE, color="red", lwd=2)
# a parte cinza em volta da reta é o intervalo de confiança da mesma (desvio padrão em ingles SE = standard error) para ver se=TRUE
