femur = read_csv("./femur.csv")
# header: "","genero","altura","femur"

femur$genero = as.factor(femur$genero)

# separar em conjunto masculino e feminino
femur_masculino = femur[femur$genero == "Male",]
femur_feminino = femur[femur$genero == "Female",]

# verifique se tem uma correlação entre tamanho do femur e altura da pessoa
# se sim plot a reta em um grafico

correlacao = function (x, y) {
	numerador = sum(x*y) - length(x)*mean(x)*mean(y)
	denominador = sqrt(sum(x^2) - length(x)*mean(x)^2) * sqrt(sum(y^2) - length(y)*mean(y)^2)
	coeficiente = numerador/denominador
	return(paste0("O coeficiente de relação entre estes dois vetores é: ", coeficiente))
}

correlacao(femur_masculino$altura, femur_masculino$femur)
correlacao(femur_feminino$altura, femur_feminino$femur)

modelo_masculino = lm(femur_masculino$femur ~ femur_masculino$altura)
modelo_feminino = lm(femur_feminino$femur ~ femur_feminino$altura)

ggplot(data=femur_masculino, aes(y=femur, x=altura)) + geom_point() + geom_smooth(method = "lm", se=TRUE, color="red", lwd=2)
ggplot(data=femur_feminino, aes(y=femur, x=altura)) + geom_point() + geom_smooth(method = "lm", se=TRUE, color="red", lwd=2)

# conclusão: o modelo masculino possui uma correlação maior que o feminino: 90% e 77% respectivamente