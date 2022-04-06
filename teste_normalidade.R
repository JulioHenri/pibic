if(!require(readxl)){install.packages("readxl");library(readxl)}


# Lendo os dados
dados1 = as.data.frame(read_excel("Bolsa CNPQ/Docs Aureliano/dados2.xlsx"))

#Visualizando o histograma de algumas variáveis

dados$intensidade = as.numeric(dados$intensidade, na=T)


hist(dados$intst)
hist(dados$f0st)
hist(dados$durz, breaks=50)

#Utilizando teste de Anderson-Darling

library(nortest)
ad.test(dados$intst)$p.value
ad.test(dados$f0st)$p.value
ad.test(dados$durz)$p.value
