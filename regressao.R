Regress <-read.table("Regress.csv", header=TRUE, sep=";")

cor (Regress)

summary (Regress)



modelo=lm(Vendas ~ Gjornal + Gmdireta + GTV, data=Regress)



summary(modelo)



residuos = modelo$residuals

hist(residuos)

qqnorm(residuos)

qqline(residuos)

shapiro.test(residuos)



residuos < - modelo$residuals


library (olsrr)

olsrr::ols_step_both_p(modelo)

----------------------------
imovel <-read.table("Imovel.CSV", header=TRUE, sep=";")
head(imovel)
cor (imovel)
view(imovel)
summary (imovel)

modelo=lm(Valor ~ Area + Idade + Energia, data=imovel)



summary(modelo)



residuos = modelo$residuals

hist(residuos)

qqnorm(residuos)

qqline(residuos)

shapiro.test(residuos)



residuos < - modelo$residuals


library (olsrr)

olsrr::ols_step_both_p(modelo)
----------------------------
biscobis <-read.table("Biscobis.CSV", header=TRUE, sep=";")
head(biscobis)
cor (biscobis)
view(biscobis)
summary (biscobis)



modelo=lm(nivel_uso ~ nivel_preco + flexibilidade_negociacao + servicos_prestados + forca_vendas , data=biscobis)



summary(modelo)



residuos = modelo$residuals

hist(residuos)

qqnorm(residuos)

qqline(residuos)

shapiro.test(residuos)



residuos < - modelo$residuals


library (olsrr)

olsrr::ols_step_both_p(modelo)

