dados = read.table('./pinguim.csv', header=TRUE, sep=",");

# estrutura dos dados
str(dados)

# resumo dos dados
summary(dados)


# filtro para embaralhar dados
amostra = sample(1:344, size=344, replace=FALSE)
amostra


# acessar quantos pinguins sao de cada especie
tabela = table(dados$species)

# plotar um gráfico mostrando quais especie aparecem mais no conjunto de dados
barplot(tabela)


# pinguins de cada sexo
penguinsBySex = table(dados$sex)

barplot(penguinsBySex)

# obter em qual(is) posição aparece a ocorrencia
faltantes = which(dados$sex == ".")

dados = dados[-faltantes,]

# pinguins de cada sexo
penguinsBySex = table(dados$sex)

barplot(penguinsBySex)

# as.factor = transformar os dados em categoria
dados$sex = as.factor(dados$sex)

faltantes = which(is.na(dados$sex))
dados = dados[-faltantes,]; # remover dados faltantes

# pinguins de cada sexo
penguinsBySex = table(dados$sex)

barplot(penguinsBySex)

summary(dados)

# categorizar os dados de qual ilha cada pinguim habita
dados$island = as.factor(dados$island)
summary(dados)

# grammer of graphics plot 2 => ggplot2
# aes => estetica e nao os bangue de cripto
# aes = mapeamento estetico usado para preenchimento baseado em variaveis, isto é, o x é a ilha e a coloração é baseado
# na variavel de especies (colora pela especie)
ggplot(data = dados, mapping = aes(x = island, fill= species))+geom_bar()+theme_replace()

      
ggplot(data= dados, mapping= aes(x=culmen_length_mm, y=flipper_length_mm, color=species))+geom_jitter()+theme_minimal()
