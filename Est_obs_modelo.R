### Estimando as observações a partir do modelo ####
# Autor: Carlos Antônio Zarzar
# 09/11/2019
# Referencias: https://pt.wikipedia.org/wiki/Coeficiente_de_determina%C3%A7%C3%A3o
#------------------------#------------------------#------------------------
# Exemplo de um modelo linear
x <- 1:100
a = 0.3
b = 2
r2 <- .823
(y <- a*x+b)
plot(x,y,type = "l")
# Soma dos quadrados explicado (SQexp) = sum( (ŷ- mean(y))^2 )
# , que calcula a parte que não é explicada pelo modelo
SQexp <- sum( (y - mean(y))^2 )/(length(y)-1)
SQexp <- var(y)
# Soma dos quadrados totais R2 = SQexp/SQt = (SQt - SQres)/SQt = 1 - SQres/SQt
SQt <- SQexp/r2
# Residual standard error
rse <- sqrt(SQt/length(y)) # Uma medidada de variabilidade dos dados
y_obs <- y+rnorm(length(y),0,rse) # Colocando a variabilidade calculada no valor preditor (ŷ)
cbind(y,y_obs)
hist(y_obs, nclass = 20)
plot(x,y,type = "l")
points(x,y_obs,col=2)

#------------------------#------------------------#------------------------
# Exemplo de um modelo não linear
## Modelo de Richards
x <- 1:100
a = 2
b = .3
c = .2
d = 5
y <- a/(1+b*exp(-c*x))^1/d
y
plot(x,y,type = "l")
r2 <- .725
SQexp <- sum( (y - mean(y))^2 )/(length(y)-1)
SQexp <- var(y)
# Soma dos quadrados totais R2 = SQexp/SQt = (SQt - SQres)/SQt = 1 - SQres/SQt
SQt <- SQexp/r2
# Residual standard error
rse <- sqrt(SQt/length(y)) # Uma medidada de variabilidade dos dados
y_obs <- y+rnorm(length(y),0,rse) # Colocando a variabilidade calculada no valor preditor (ŷ)
cbind(y,y_obs)
hist(y_obs, nclass = 20)
plot(x,y,type = "l")
points(x,y_obs,col=2)
#------------------------#------------------------#------------------------

