############ TRABALHO FINAL - LINGUAGEM R ############

# Informando o diret?rio
#setwd("C:/Users/Bernardo/OneDrive/Documents/Data Science/MBA/Linguagem R/Material-20210204/Trabalho Final")

# Instala��o dos pacotes
#install.packages("tidyverse")
library(tidyverse)
#install.packages("DataExplorer")
library(DataExplorer)

# Leitura da base de dados
diagnostico=read.csv("data.csv")

###################################
##QUAL A PERGUNTA A SER RESPONDIDA?
###################################

#Resposta:
#Dado as m�tricas dos n�cleos da c�lulas, como saber se o c�ncer � maligno ou benigno?


####### Sanity Check

# A �nica tabela utilizada para a an�lise foi a tabela fornecida para o estudo,
# contendo 30 medidas num�ricas de cada registro e o seu diagn�stico (Maligno ou Benigno) 

introduce(diagnostico)
# A tabela possui um total de 569 linhas e 33 colunas, sendo que uma delas cont�m apenas valores nulos

plot_intro(diagnostico) # representa��o gr�fica sobre a base de dados

plot_missing(diagnostico) 
# Apenas a �ltima coluna cont�m valores nulos (em todos os registros)

### Remover coluna X (sem valores)
diagnostico<-subset(diagnostico, select = -c(X))
plot_missing(diagnostico)

### A chave da tabela � a coluna ID
### Confirmando que n�o h� duplicidade de ID
idunico<-diagnostico %>%
  group_by(diagnostico$id) %>%
  count()

### Estat�sticas descritivas das colunas num�ricas
summary(diagnostico)

### Analisando Vari�veis Categ�ricas e Num�ricas
g<-plot_bar(diagnostico)
plot_histogram(diagnostico)

########################
#Qual é a variável alvo?
########################

#Resposta
#A partir da an�lise das vari�veis, consideramos que a vari�vel alvo seria "diagnosis"
#pelo fato de ser a vari�vel que responde nossa pergunta. Sem ela, a base fica sem informa��es suficientes 
#para tomada de decis�o



#########################################
#Qual é o comportamento da variável alvo?
#########################################
#Resposta
cont_diagnosis<-diagnostico %>% 
    count(diagnostico$diagnosis)
# Dado que temos apenas 1 variável categórica, ela se comporta da seguinte maneira:
#   M - 212
#   B - 357


#############################################
#Quais variáveis mais se relacionam entre si?
#############################################
plot_correlation(diagnostico,type = "all")

diagnostico2<-diagnostico
diagnostico2$diagnosis<-ifelse(diagnostico$diagnosis=="M",0,1)

# calcular coeficiente de correla��o
Correlacao <- cor(diagnostico2, use = "complete.obs")
# instala��o do pacote
install.packages("corrplot")
library(corrplot)

corrplot(Correlacao, method = "color", cl.pos = "b", type = "lower", addgrid.col = "white",
         addCoef.col = "white", tl.col = "black", tl.cex = 0.7, number.cex = 0.7, cl.cex = 0.7)
# Os conjuntos de vari�veis com maior correla��o entre si s�o "area_mean" com "radius_mean",
# "area_mean" com "perimeter_mean" e "perimeter_worst" com "radius_worst", todas com correla��o igual a 0,99

#Resposta
#Dada a matriz de correla��o, podemos elencar algumas vari�veis com boas 
#correla��es com a vari�vel alvo, buscando menor redund�ncia entre elas:
# radius_mean
# concave.points_mean
# perimeter_mean
# concavity_mean

p1 <- ggplot(diagnostico, aes(x=diagnosis, y=radius_mean,fill=diagnosis)) +geom_boxplot()
p2 <- ggplot(diagnostico, aes(x=diagnosis, y=concave.points_mean,fill=diagnosis)) +geom_boxplot()
p3 <- ggplot(diagnostico, aes(x=diagnosis, y=perimeter_mean,fill=diagnosis)) +geom_boxplot()
p4 <- ggplot(diagnostico, aes(x=diagnosis, y=concavity_mean,fill=diagnosis)) +geom_boxplot()
p1
p2
p3
p4

#####################################################
#Quais vari�veis seriam selecionadas para a modelagem?
#####################################################

#Resposta
#Selecionamos as m�tricas que tem maior coeficiente de correla��o com a vari�vel 
#alvo. Pegamos a m�dia pois o pior valor pode ser muito enviesado 
#para um resultado maligno.