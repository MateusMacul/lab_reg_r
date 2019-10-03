################################################################################
############                   LABORATÓRIO                      ################
############            ANÁLISE DE REGRESSÃO NO R               ################
################################################################################

# Limpar o ambiente -------------------------------------------------------


rm(list=ls())


# Instalando os pacotes necessários ---------------------------------------

install.packages("sp") 
install.packages("corrplot") 
install.packages("PerformanceAnalytics") 
install.packages("nortest") 
install.packages("betas") 
install.packages("spdep") 
install.packages("maptools") 
install.packages("Hmisc") 
install.packages("mapview") 
install.packages("car") 
install.packages("ggplot2") 
install.packages("dplyr") 
install.packages("spdep") 
install.packages("rgdal") 
install.packages("cartography") 
install.packages("lmtest") 

# carregando os pacotes ---------------------------------------------------
# forma 1
require(foreign)
require(rgdal)
require(spdep)
require(sp)
require(maptools)
require(dplyr)
# fomra 2
library("corrplot")
library("Hmisc")
library("PerformanceAnalytics")
library("nortest")
library("betas")
library("mapview")
library("cartography")
library("ggplot2")
library("car")
library("lmtest")

# definindo o diretorio de trabalho (working directory) -----------------------

setwd("C:/Users/Queimadas/OneDrive - inpe.br/Disciplinas/analiseespacial/Laboratorio de Reg R")

# abrindo os dados --------------------------------------------------------

dados <- readOGR("dados/dados_lab_reg.shp")

# analisar o formato de dados

dados

# olhando os dados --------------------------------------------------------
# existem diversas formas:
str(dados@data)
head(dados@data)
summary(dados)
View(dados@data)

length(dados)
# olhando no espaço
mapview(dados)

'
METADATA dos dados:

id       : id de cada célula
incre17_p: proporção de incremento de desmatamento de 2017
incre16_p: proporção de incremento de desmatamento de 2016
incre15_p: proporção de incremento de desmatamento de 2015
incre14_p: proporção de incremento de desmatamento de 2014
br_d     : distancia da BR163 em metros
estr     : distancia das estradas vicinais normalizada de 0 a 1
areaim   : tamanho médio dos imóveis dentro de cada célula normalizado entre 0 a 1
d13      : distancia do desmatamento acumulado até 2013 normalizada de 0 a 1
deg      : distancia de degradação florestal normalizada de 0 a 1
ast      : distancia de assentamento rurais normalizada de 0 a 1
au       : distancia de área urbana normalizada de 0 a 1
uc       : proporção de unidade de conservação dentro de cada célula
abert    : proporção de área aberta (pastagem e agricultura) dentro de cada célula
'
# contruindo novas variáveis ----------------------------------------------

# criando a variável y = incrmento acumulado de 2014 a 2017
dados$y <- dados$incre14_p + dados$incre15_p + dados$incre16_p + dados$incre17_p

# aplicando a tranformação logit na variável y
dados$ylgit <- log(dados$y/(1-dados$y))

summary(dados$y)

summary(dados)

# normalizando a variável de distancia da BR163 (br_d)

limiar = 50000
for (i in 1:length(dados$br_d)){ # 
  if (dados$br_d[i] <= limiar){
    dados$br[i] = dados$br_d[i]/limiar # invertido
  } else {
    dados$br[i] = 1 # vai para zero pq ta invertido. se não, seria 1
  }
}

summary(dados)
# exportar os dados caso necessário

writeOGR(obj = dados, dsn ="dados"  ,layer = "dados_exemplo", driver = "ESRI Shapefile")

# selecionando os dados --------------------------------------------------

# Selecionando todos os dados que não tem y = 0 e as colunas que interessam
# para selecionar usamos a indexação 'dados[linhas, colunas]'

dados.nozero <- dados[dados$y != 0, c('id','y','ylgit','br','estr','areaim', 
                                      'd13', 'deg', 'ast','au', 'uc', 'abert')]

# analisando o resultado dos dados selecionados
length(dados.nozero)
str(dados.nozero@data)
View(dados.nozero@data)
summary(dados.nozero)

mapview(dados, col.regions = "grey") + mapview(dados.nozero, col.regions = "red")

# plotando ----------------------------------------------------------------

hist(dados.nozero$y) # histograma
# histograma com frequencia relativa
h_inc_d <- hist(dados.nozero$y, plot = F)
h_inc_d$counts <- h_inc_d$counts/sum(h_inc_d$counts)
plot(h_inc_d,xlab = "y",ylab = "Frequência relativa",
     main = "Histograma do beta para y", freq = T)

boxplot(dados.nozero@data[, -1])# boxplot de todos as colunas menos a coluna 'id'
plot(dados.nozero$y, dados.nozero$br) # dispersao


# amostragem --------------------------------------------------------------
# agrupar para estratificar
agrupada <- group_by(dados.nozero@data, 
                     ntile(dados.nozero$y, 10)) # estratificando por quantis (tiles)
# a variavel 'agurpada' agora é um DataFrame
group_size(agrupada) #tamanho das amostras

amostrada <- sample_frac(agrupada, size = 0.3) # amostrando 30% (0.3) de cada quantil (tiles)

# selecionando do conjunto amostral (dados.nozero), as amostras (amostrada)

dados.amostrado <- dados.nozero[grep(c(paste(amostrada$id,collapse = "|")), 
                                     dados.nozero@data$id, ignore.case = T), ]
# olhando o resultado
mapview(dados, col.regions = "grey") + mapview(dados.nozero, col.regions = "red") +
  mapview(dados.amostrado, col.regions = "blue")

# análise de correlação ---------------------------------------------------
# duas formas para a análise de correlação

chart.Correlation(dados.amostrado@data[, -1], histogram=TRUE, pch=19, 
                  font.labels = 4, cex.labels = 10)


m<-cor(dados.amostrado@data[,-1])
corrplot(m,method = "number", type = "upper")

# contruindo o modelo -----------------------------------------------------
'
operadores para a modelagem:

operador |           exemplo      | função
---------------------------------------------------------------------------
   +     |            +x          | inclui variável
   -     |            -X          | exclui a variável
   :     |           X1:X2        | inclui a interação entre as variáveis
   *     |X1*X2 = X1 + X2 + X1:X2 | inclui as variáveis e a interação entre elas
   ^     |      (X1 + X2 +X3)^3   | inclui as variáveis e todas as interações possíveus até 3º grau
  I()    |         I(X1*X2)       | usado para realizar operações entre variáveis da forma como é escrita (X1 vezes X2)
   1     |           -1           | inclui (+1) ou exclui (-1) o intercepto 
   .     |            .           | inclui todas as variáveis presente nos dados
---------------------------------------------------------------------------
'
# contruindo um modelo

modelo <- lm(ylgit ~ . -y , data = dados.amostrado@data[, -1])
# visualisando o modelo
summary(modelo)

# uma fomra automática para encontrar as melhores variáveis para o modelo

modelo.step <- step(modelo, scale = 0 , direction = "both", trace = 0)
# visualizando o resultado
summary(modelo.step)

vif(modelo) # evitando a multcolinearidade


# plotando resultados -----------------------------------------------------

# Gráfico de dispersão do predito x Observado
ggplot()+
  geom_point(aes(y = dados.amostrado$y, x = modelo.step$fitted.values))+
  geom_smooth()+
  xlab("y' Predito")+
  ylab("y' Observado")+
  geom_abline()+
  geom_smooth(method = "lm")+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))

# Gráfico de dispersão dos resíduos do modelo x y predito
ggplot()+
  geom_point(aes(y = (modelo.step$residuals/sd(modelo.step$residuals)), x = modelo.step$fitted.values))+
  geom_hline(yintercept = 0)+
  ylab("Resíduos padronizados")+
  xlab("y Predito")+ 
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))

# Analisando os erros

dados.amostrado$erros <- modelo.step$residuals
View(dados.amostrado@data)
mapview(dados.amostrado,zcol = "erros", col.regions = colorRampPalette(colors = c("red2","white", "yellow")))

# teste para os residuos --------------------------------------------------

# homocedasticidade

bptest(modelo.step, data=dados.amostrado, studentize=F)

# normalidade
residuos <- modelo.step$residuals 
shapiro.test(residuos) 


# removendo outliers ------------------------------------------------------
dados.out <- dados.amostrado[!(dados.amostrado$id == "C13L31"), ]
#dados.out <- dados.amostrado[!(dados.amostrado$erros < -2), ]
#dados.out <- dados.out[!(dados.out$erros > 2), ]
length(dados.amostrado)
length(dados.out)

# rodando o modelo novamente sem os outliers

modelo <- lm(ylgit ~ . -y -erros, data = dados.out@data[, -1])
summary(modelo)

# rodando o stepwise para o modelo

modelo.step =  step(modelo, scale = 0 , direction = "both", trace = 0)
summary(modelo.step)

vif(modelo.step) # evitando a multcolinearidade


# plotando resultados 2 

ggplot()+
  geom_point(aes(y = dados.out$y, x = modelo.step$fitted.values))+
  geom_smooth()+
  xlab("y' Predito")+
  ylab("y' Observado")+
  geom_abline()+
  geom_smooth(method = "lm")+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))

ggplot()+
  geom_point(aes(y = (modelo.step$residuals/sd(modelo.step$residuals)), x = modelo.step$fitted.values))+
  geom_hline(yintercept = 0)+
  ylab("Resíduos padronizados")+
  xlab("y Predito")+ 
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))

# analisando os resíduos novamente sem os outliers

dados.out$erros <- modelo.step$residuals
View(dados.amostrado@data)
mapview(dados.out,zcol = "erros", col.regions = colorRampPalette(colors = c("red2","white", "yellow")))

# teste para os residuos 

# homocedasticidade

bptest(modelo.step, data=dados.amostrado, studentize=F)

# normalidade
residuos <- modelo.step$residuals 
shapiro.test(residuos) 



