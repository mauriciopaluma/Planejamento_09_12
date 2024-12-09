### Problema 4.14 - Edição 8:
bloco = rep(c(1,2,3,4),4)
Stirr = c(5,5,5,5,10,10,10,10,15,15,15,15,20,20,20,20)
y = c(8,4,5,6,
      14,5,6,9,
      14,6,9,2,
      17,9,3,6)
cbind(Stirr, bloco, y)

require(ExpDes.pt)

plot(y~Stirr, pch = 19)
boxplot(y~Stirr, pch = 19)

bartlett.test(y~Stirr)

saida1 = dbc(Stirr, bloco, y, hvar = "oneillmathews", mcomp = "tukey")

y1 = c(8,4,5,6,
      14+20,5+20,6+20,9+20,
      14+20,6+20,9+20,2+20,
      17,9,3,6)
plot(y1~Stirr, pch = 19)
boxplot(y1~Stirr, pch = 19)

bartlett.test(y1~Stirr)

saida2 = dbc(Stirr, bloco, y1, quali = TRUE, 
             hvar = "oneillmathews", mcomp = "tukey")

saida3 = dbc(Stirr, bloco, y1, quali = FALSE, 
             hvar = "oneillmathews", mcomp = "tukey")

coef = c(-36.5625, 10.6025, -0.4175)             

coef(lm(y1~Stirr+I(Stirr^2)))

saida_lm = lm(y1~Stirr+I(Stirr^2))

plot(y1~Stirr, pch = 19)
lines(spline(Stirr, fitted(saida_lm)), n = 201, lwd = 2, col = "blue")

coef(saida_lm)
predict(saida_lm)

predict(saida_lm, list(Stirr = 16.4, Stirr2 = 16.4^2)) #Valor estimado para Stirr = 16,4

y2 = c(8,4,5,6,
       14+18,5+18,6+12,9+12,
       14+2,6+3,9+12,2+12,
       17,9,3,6)
bartlett.test(y2~Stirr)
