# Pacotes
if(!require(readxl)){install.packages("readxl");library(readxl)}
if(!require(ggplot2)){install.packages("ggplot2");library(ggplot2)}
if(!require(ggthemes)){install.packages("ggthemes");library(ggthemes)} 
if(!require(gridExtra)){install.packages("gridExtra");library(gridExtra)}
if(!require(ggpubr)){install.packages("ggpubr");library(ggpubr)}

# Lendo os dados
dados = as.data.frame(read_excel("Bolsa CNPQ/Docs Aureliano/dados2.xlsx"), na = T )

#Transformando natureza dos dados
dados$vogal = as.factor(dados$vogal)
dados$f0st = as.numeric(dados$f0st)

summary(dados$vogal)
summary(dados$f0st)

str(dados$vogal)
str(dados$f0st)

#Separando entre os municipios
mazagao = dados[which(dados[,13]=='Mazagão'),]
oiapoque = dados[which(dados[,13]=='Oiapoque'),]

# Acentos Oiapoque
oxi_oiapoque =  oiapoque[which(oiapoque[,9]=="oxitona"),]
par_oiapoque =  oiapoque[which(oiapoque[,9]=="paroxitona"),]
propa_oiapoque =  oiapoque[which(oiapoque[,9]=="proparoxitona"),]

# Acentos Mazagao
oxi_mazagao =  mazagao[which(mazagao[,9]=="oxitona"),]
par_mazagao =  mazagao[which(mazagao[,9]=="paroxitona"),]
propar_mazagao =  mazagao[which(mazagao[,9]=="proparoxitona"),]

# Por escolaridade
dadosfund = dados[which(dados[,10]=="fundamental"),]
dadosmed = dados[which(dados[,10]=="medio"),]
dadossup = dados[which(dados[,10]=="superior"),]

# Todas as Variedades
ggline(data = dados, x= "vogal",y="durz", add = c("mean"),
       color = "Variedade", shape = "Variedade", 
       facet.by = c("acento","modalidade"), size = 0.8,ylab = "f0st", 
       xlab = "Vogal", label.select = "Variedade")

# mazagao
ma_f01 = ggline(data = mazagao, x= "vogal",y="f0st", add = c("mean"), 
               facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
               color = "#31A354",xlab = "Mazagão")

# oiapoque
oi_f01 = ggline(data = oiapoque, x= "vogal",y="f0st", add = c("mean"), 
               facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
               color = "#31A354",xlab = "Oiapoque")


grid.arrange(ma_f01, oi_f01,bottom = "Vogais",left = "F0(st)" )

## Intensidade para todas as Variedades  


ggline(data = dados, x= "vogal",y="intst", add = c("mean"),
       color = "Variedade", shape = "Variedade", 
       facet.by = c("acento","modalidade"), size = 0.8,ylab = "Int_N",
       xlab = "Vogal")

# mazagao
ma_int = ggline(data = mazagao, x= "vogal",y="intst", add = c("mean"), 
                facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                color = "#31A354",xlab = "Mazagão")

# oiapoque
oi_int = ggline(data = oiapoque, x= "vogal",y="intst", add = c("mean"), 
                facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                color = "#31A354",xlab = "Oiapoque")

grid.arrange(ma_int,oi_int,bottom = "Vogais",left = "Int_N")

## Duracao para todas as Variedades  

ggline(data = dados, x= "vogal",y="durz", add = c("mean"),
       color = "Variedade", shape = "Variedade", 
       facet.by = c("acento","modalidade"), size = 0.8,ylab = "Dur(z-score)")

# magazao
ma_dur = ggline(data = mazagao, x= "vogal",y="durz", add = c("mean"), 
                facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                color = "#31A354",xlab = "Mazagão")

# oiapoque
oi_dur = ggline(data = oiapoque, x= "vogal",y="durz", add = c("mean"), 
                facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                color = "#9E9AC8",xlab = "Oiapoque")

grid.arrange(ma_dur, oi_dur,bottom = "Vogais",left = "Dur(z-score)")

## F0 para todas as Variedades  -  Ensino Fundamental

## Fundamental
dadosfundmazagao = dadosfund[which(dadosfund[,13]=="Mazagão"),]
dadosfundoiapoque = dadosfund[which(dadosfund[,13]=="Oiapoque"),]

ggline(data = dadosfund, x= "vogal",y="f0st", add = c("mean"),
       color = "Variedade", shape = "Variedade", 
       facet.by = c("acento","modalidade"), size = 0.8,ylab = "F0(st)")

# mazagao Fundamental
ma_fund_f0 = ggline(data = dadosfundmazagao, x= "vogal",y="f0st", add = c("mean"), 
                    facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                    color = "#31A354",xlab = "Mazagão")

# oiapoque Fundamental
oi_fund_f0 = ggline(data = dadosfundoiapoque, x= "vogal",y="f0st", add = c("mean"), 
                    facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                    color = "#9E9AC8",xlab = "Oiapoque")

grid.arrange(ma_fund_f0,oi_fund_f0,bottom = "Vogais",left = "F0(st)")

## Intensidade para todas as Variedades Ensin Fundamental
## Fundamental

ggline(data = dadosfund, x= "vogal",y="intst", add = c("mean"),
       color = "Variedade", shape = "Variedade", 
       facet.by = c("acento","modalidade"), size = 0.8,ylab = "Int_N")


# mazagao Fundamental
ma_fund_int = ggline(data = dadosfundmazagao, x= "vogal",y="intst", add = c("mean"), 
                     facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                     color = "#31A354",xlab = "Mazagão")

# Oiapoque Fundamental
oi_fund_int = ggline(data = dadosfundoiapoque, x= "vogal",y="intst", add = c("mean"), 
                     facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                     color = "#9E9AC8",xlab = "Oiapoque")

grid.arrange(ma_fund_int,oi_fund_int,bottom = "Vogais"
             ,left = "Int_N")

## Duracao para todas as Variedades  Ensino Fundamental
## Fundamental

ggline(data = dadosfund, x= "vogal",y="durz", add = c("mean"),
       color = "Variedade", shape = "Variedade", 
       facet.by = c("acento","modalidade"), size = 0.8,ylab = "Dur(z-score)")

# mazagao Fundamental
ma_fund_dur = ggline(data = dadosfundmazagao, x= "vogal",y="durz", add = c("mean"), 
                     facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                     color = "#31A354",xlab = "Mazagão")

# parintins Fundamental
oi_fund_dur = ggline(data = dadosfundoiapoque, x= "vogal",y="durz", add = c("mean"), 
                     facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                     color = "#9E9AC8",xlab = "Oiapoque")

grid.arrange(ma_fund_dur,oi_fund_dur,bottom = "Vogais"
             ,left = "Dur(z-score)")

## F0 para todas as Variedades  Ensino Medio
## Medio
dadosmedmazagao = dadosmed[which(dadosmed[,13]=="Mazagão"),]
dadosmedoiapoque = dadosmed[which(dadosmed[,13]=="Oiapoque"),]

ggline(data = dadosmed, x= "vogal",y="f0st", add = c("mean"),
       color = "Variedade", shape = "Variedade", 
       facet.by = c("acento","modalidade"), size = 0.8,ylab = "F0(st)")


# mazagao Medio
ma_med_f0 = ggline(data = dadosmedmazagao, x= "vogal",y="f0st", add = c("mean"), 
                   facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                   color = "#31A354",xlab = "Mazagão")

# oiapoque Medio
oi_med_f0 = ggline(data = dadosmedoiapoque, x= "vogal",y="f0st", add = c("mean"), 
                   facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                   color = "#9E9AC8",xlab = "Oiapoque")

grid.arrange(ma_med_f0,oi_med_f0,
             bottom = "Vogais",left = "F0(st)")

## Intensidade para todas as Variedades  Ensino medio
## Medio

ggline(data = dadosmed, x= "vogal",y="intst", add = c("mean"),
       color = "Variedade", shape = "Variedade", 
       facet.by = c("acento","modalidade"), size = 0.8,ylab = "Int_N")

# mazagao Medio
ma_med_int = ggline(data = dadosmedmazagao, x= "vogal",y="intst", add = c("mean"), 
                    facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                    color = "#31A354",xlab = "Mazagão")

# oiapoque Medio
oi_med_int = ggline(data = dadosmedoiapoque, x= "vogal",y="intst", add = c("mean"), 
                    facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                    color = "#9E9AC8",xlab = "Oiapoque")

grid.arrange(ma_med_int,oi_med_int,
             bottom = "Vogais",left = "Int_N")

## [] Duracao para todas as Variedades  Ensino medio
## Medio

ggline(data = dadosmed, x= "vogal",y="durz", add = c("mean"),
       color = "Variedade", shape = "Variedade", 
       facet.by = c("acento","modalidade"), size = 0.8,ylab = "Dur(z-score)")

# mazagao Medio
ma_med_dur = ggline(data = dadosmedmazagao, x= "vogal",y="durz", add = c("mean"), 
                    facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                    color = "#31A354",xlab = "Mazagão")

oi_med_dur = ggline(data = dadosmedoiapoque, x= "vogal",y="durz", add = c("mean"), 
                    facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                    color = "#31A354",xlab = "Oiapoque")

grid.arrange(ma_med_dur,oi_med_dur,
             bottom = "Vogais",left = "Dur(z-score)")

## F0 para todas as Variedades Superior
## Superior
dadossupmazagao = dadossup[which(dadosmed[,13]=="Mazagão"),]
dadossupoiapoque = dadossup[which(dadosmed[,13]=="Oiapoque"),]

ggline(data = dadossup, x= "vogal",y="f0st", add = c("mean"),
       color = "Variedade", shape = "Variedade", 
       facet.by = c("acento","modalidade"), size = 0.8,ylab = "F0(st)")


# mazagao superior
ma_sup_f0 = ggline(data = dadossupmazagao, x= "vogal",y="f0st", add = c("mean"), 
                   facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                   color = "#31A354",xlab = "Mazagão")

# oiapoque Medio
oi_sup_f0 = ggline(data = dadossupoiapoque, x= "vogal",y="f0st", add = c("mean"), 
                   facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                   color = "#9E9AC8",xlab = "Oiapoque")

grid.arrange(ma_sup_f0,oi_sup_f0,
             bottom = "Vogais",left = "F0(st)")

## Intensidade para todas as Variedades  superior
## Superior

ggline(data = dadossup, x= "vogal",y="intst", add = c("mean"),
       color = "Variedade", shape = "Variedade", 
       facet.by = c("acento","modalidade"), size = 0.8,ylab = "Int_N")

# mazagao superior
ma_med_int = ggline(data = dadossupmazagao, x= "vogal",y="intst", add = c("mean"), 
                    facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                    color = "#31A354",xlab = "Mazagão")

# oiapoque Superior
oi_med_int = ggline(data = dadossupoiapoque, x= "vogal",y="intst", add = c("mean"), 
                    facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                    color = "#9E9AC8",xlab = "Oiapoque")

grid.arrange(ma_med_int,oi_med_int,
             bottom = "Vogais",left = "Int_N")


## Duracao para todas as Variedades  Ensino medio
## Medio

ggline(data = dadossup, x= "vogal",y="durz", add = c("mean"),
       color = "Variedade", shape = "Variedade", 
       facet.by = c("acento","modalidade"), size = 0.8,ylab = "Dur(z-score)")

# mazagao Superior
ma_sup_dur = ggline(data = dadossupmazagao, x= "vogal",y="durz", add = c("mean"), 
                    facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                    color = "#31A354",xlab = "Mazagão")

oi_sup_dur = ggline(data = dadossupoiapoque, x= "vogal",y="durz", add = c("mean"), 
                    facet.by = c("acento","modalidade"), size = 0.8,ylab = "",
                    color = "#31A354",xlab = "Oiapoque")

grid.arrange(ma_sup_dur,oi_sup_dur,
             bottom = "Vogais",left = "Dur(z-score)")
 