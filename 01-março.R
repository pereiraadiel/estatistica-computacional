library(GGally)
library(ggplot2)

steven = c(0,1,0)
garnit = c(0,0,1)
resultado = sample(0:1, size=3, replace=TRUE)

vitoriasSteven = 0
vitoriasGarnit = 0


for(k in 1:1e5) {
	resultado = sample(0:1, size=3, replace=TRUE)
	while(sum(resultado == steven) < 3 && sum(resultado == garnit) < 3){
		resultado = c(resultado[2:3], sample(0:1, size=1, replace=TRUE))
	}
	if(sum(resultado == steven) == 3){
		vitoriasSteven = vitoriasSteven + 1
	}else{
		vitoriasGarnit = vitoriasGarnit + 1
	}
}
