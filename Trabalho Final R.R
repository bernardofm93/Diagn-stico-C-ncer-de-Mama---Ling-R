############ TRABALHO FINAL - LINGUAGEM R ############

# Informando o diret?rio
#setwd("C:/Users/Bernardo/OneDrive/Documents/Data Science/MBA/Linguagem R/Material-20210204/Trabalho Final")

# Instalação dos pacotes
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
#Dado as métricas dos núcleos da células, como saber se o câncer é maligno ou benigno?


####### Sanity Check

# A única tabela utilizada para a análise foi a tabela fornecida para o estudo,
# contendo 30 medidas numéricas de cada registro e o seu diagnóstico (Maligno ou Benigno) 

introduce(diagnostico)
# A tabela possui um total de 569 linhas e 33 colunas, sendo que uma delas contém apenas valores nulos

plot_intro(diagnostico) # representação gráfica sobre a base de dados

plot_missing(diagnostico) 
# Apenas a última coluna contém valores nulos (em todos os registros)

### Remover coluna X (sem valores)
diagnostico<-subset(diagnostico, select = -c(X))
plot_missing(diagnostico)

### A chave da tabela é a coluna ID
### Confirmando que não há duplicidade de ID
idunico<-diagnostico %>%
  group_by(diagnostico$id) %>%
  count()

### Estatísticas descritivas das colunas numéricas
summary(diagnostico)

### Analisando Variáveis Categóricas e Numéricas
g<-plot_bar(diagnostico)
plot_histogram(diagnostico)

########################
#Qual Ã© a variÃ¡vel alvo?
########################

#Resposta
#A partir da análise das variáveis, consideramos que a variável alvo seria "diagnosis"
#pelo fato de ser a variável que responde nossa pergunta. Sem ela, a base fica sem informações suficientes 
#para tomada de decisão



#########################################
#Qual Ã© o comportamento da variÃ¡vel alvo?
#########################################
#Resposta
cont_diagnosis<-diagnostico %>% 
    count(diagnostico$diagnosis)
# Dado que temos apenas 1 variÃ¡vel categÃ³rica, ela se comporta da seguinte maneira:
#   M - 212
#   B - 357


#############################################
#Quais variÃ¡veis mais se relacionam entre si?
#############################################
plot_correlation(diagnostico,type = "all")

diagnostico2<-diagnostico
diagnostico2$diagnosis<-ifelse(diagnostico$diagnosis=="M",0,1)

# calcular coeficiente de correlação
Correlacao <- cor(diagnostico2, use = "complete.obs")
# instalação do pacote
install.packages("corrplot")
library(corrplot)

corrplot(Correlacao, method = "color", cl.pos = "b", type = "lower", addgrid.col = "white",
         addCoef.col = "white", tl.col = "black", tl.cex = 0.7, number.cex = 0.7, cl.cex = 0.7)
# Os conjuntos de variáveis com maior correlação entre si são "area_mean" com "radius_mean",
# "area_mean" com "perimeter_mean" e "perimeter_worst" com "radius_worst", todas com correlação igual a 0,99

#Resposta
#Dada a matriz de correlação, podemos elencar algumas variáveis com boas 
#correlações com a variável alvo, buscando menor redundância entre elas:
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
#Quais variáveis seriam selecionadas para a modelagem?
#####################################################

#Resposta
#Selecionamos as métricas que tem maior coeficiente de correlação com a variável 
#alvo. Pegamos a média pois o pior valor pode ser muito enviesado 
#para um resultado maligno.