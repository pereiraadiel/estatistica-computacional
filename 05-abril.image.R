install.packages('jpeg')
library(jpeg)

imagem = readJPEG("image.jpg")
# largura 200
# altura 200
largura = 1024
altura = 768

str(imagem)

red = as.vector(imagem[,,1])
green = as.vector(imagem[,,2])
blue = as.vector(imagem[,,3])

x = rep(1:largura, each=altura) # largura
y = rep(altura:1, times=largura) # altura

dados = data.frame(x=x, y=y, red=red, green=green, blue=blue)

modelo = kmeans(dados[, 3:5], centers=5) # 3 aglomerados
modelo$centers

modelo$cluster

dados$aglomerados = factor(modelo$cluster)

ggplot(dados, aes(x=x, y=y, col=aglomerados))+geom_point()+scale_color_manual(values=rgb(modelo$centers))
