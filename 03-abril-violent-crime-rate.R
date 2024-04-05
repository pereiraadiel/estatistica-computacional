library(rvest)
library(dplyr)
library(ggplot2)

url = "https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_violent_crime_rate"

html = read_html(url)

dados = html |> html_element("table") |> html_table() |> as.data.frame()

str(dados)

# renomear as colunas que possuem ESPAÇO no nome 
colnames(dados)[c(2,6)] = c("violent.crime", "assault")

?map_data # função do pacote ggplot2

estados = map_data("state") # estados do estados unidos

dados$region = tolower(dados$Location)

dados = dados[, -1] # remover a coluna Location

# unir as tabelas dados e estados(do mapa)
dadosGerais = left_join(estados, dados, by = "region")

str(dadosGerais)

# plotar o mapa
ggplot(data = dadosGerais)+geom_polygon(aes(x=long, y=lat, group=group, fill=Homicide), col="white")+scale_fill_gradient(low='#9933cc', high='#9966ff')+theme(title=element_text(family="mono"), legend.position = "bottom")+labs(title="taxa de homicidio")+theme_void()
