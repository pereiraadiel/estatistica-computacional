dados = read.table(file="./pinguim.csv", header=T, sep=',')

head(dados)

str(dados)

unique(dados$sex)

# remover dados incorretos
incorretPositions = which(dados$sex == ".")
dados = dados[-incorretPositions, ]

# remover dados faltantes
unavailablePositions = which(is.na(dados$sex));
dados = dados[-unavailablePositions,]

# resumo estatistico dos dados
summary(dados)

unique(dados$island)


# transformar variaveis categoricas em categorias
dados$species = as.factor(dados$species)
dados$sex = as.factor(dados$sex)
dados$island = as.factor(dados$island)

numeroObservacoes = nrow(dados)

# amostra para embaralhamento
amostra = sample(1:numeroObservacoes, size=numeroObservacoes, replace=FALSE)

# dados embaralhados
dadosAmostral = dados[amostra, ]

amostraTreino = round(numeroObservacoes*0.8)


# conjuto para treino
treino = dadosAmostral[1: amostraTreino, ]

# conjunto para teste
teste = dadosAmostral[(amostraTreino + 1): numeroObservacoes, ]

# install.packages("ggplot2")
# library(ggplot2)
ggplotx(data= treino, aes(x = species, fill=sex))+geom_bar()+labs(x= "espécies", y="frequência", title="Frequência de cada uma das espécies", fill="sexo")+scale_fill_manual(values= c("darkorange", "darkorchid"), labels= c("fêmea", "macho"))

#
# geom_bar # estilo de grafico
# labs  # modificar legendas
# scale_* # scale = modificar cores

# grafico de boxplot

ggplot(data= treino, aes(x = sex, y = body_mass_g))+geom_boxplot()+facet_wrap(~species)

# facet_wrap = criar faces para cada especie
# o til ~ significa em função = em função de especies => ~species

# Grafico em partes

graficoBase = ggplot(data = treino, aes(x=sex, y=body_mass_g))
graficoFacetadoEmFuncaoDeEspecies = graficoBase+geom_boxplot()+facet_wrap(~species)+labs(x= "sexos", y="peso corporal", title="Relação peso/sexo entre as espécies", fill="sexo")

graficoFacetadoEmFuncaoDeEspecies

# tamanho do bico e profundidade de bico de cada especie
ggplot(data= treino, aes(x=culmen_length_mm, y=culmen_depth_mm, color=species, size=1.25))+geom_point()+scale_color_manual(values=c("darkorange", "cyan4", "darkorchid"))

# distribuição do sexo em funçao da especie
ggplot(data= treino, aes(x=culmen_length_mm, y=culmen_depth_mm, color=sex, size=1.25))+geom_point()+scale_color_manual(values=c("darkorange", "cyan4", "darkorchid"))+facet_wrap(~species)

# distribuição da especie em funçao da ilha
ggplot(data= treino, aes(x=culmen_length_mm, y=culmen_depth_mm, color=species, size=1.25))+geom_point()+scale_color_manual(values=c("darkorange", "cyan4", "darkorchid"))+facet_wrap(~island)

# implemente uma função que o usuario entre o numero de vizinhos mais proximos e a função retorne a classificação do pinguim

# distancia euclidiana: sqrt((x2-x1)^2 + (y2-y1)^2)

respostas = c()

for(j in 1:nrow(teste)){
		distancias = c()
		for(i in 1:nrow(treino)){
			distancias[i] = sqrt(sum((teste[j, c("culmen_length_mm", "culmen_depth_mm")]-treino[i, c("culmen_length_mm", "culmen_depth_mm")])^2))
    order(distancias)
  }
	respostas[j] = as.character(treino[order(distancias)[1], "species"])
}

respostas

# order = exibe a ordem em que os elementos aparecem do menor pro maior

mean(respostas == teste$species)
