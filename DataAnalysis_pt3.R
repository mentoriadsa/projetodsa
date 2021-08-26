# PROJETO MENTORIA DSA - 2021 #
#
# PROBLEMA:  Prever quando um país estará com a pandemia sob controle.
#
# Pontos a serem explorados:
#  - Qual métrica será utilizada para definir com um país conquistou o nível de segurança.
# 
# Dataset desenvolvido a partir de informações de: https://github.com/CSSEGISandData/COVID-19
# 
# Script responsável em lidar com os imputation das variáveis NA

setwd("C:/Users/guice/Desktop/ProjetoMentoriaDSA")

# Bibliotecas
install.packages("dplyr")
install.packages("tidyr")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("readr")
install.packages("tidyverse")
install.packages("mice")
install.packages("lares")
install.packages("psych")
library(lares)
library(mice)
library(tidyverse)
library(dplyr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(readr)
library(psych)
library(caret)


dataset <- read_csv("dataset_sem_localidade_mundial.csv")

# Validando as medidas centrais do dataset
summary(dataset)
# Conforme análise do summary, existem valores negativos no dataset
# como não sabemos a razão, esses mesmos valores serão removidos.

colunas_numericas <- unlist(lapply(dataset, is.numeric))
colunas_valores_negativos <- apply(dataset[,colunas_numericas], 1, function(row) any(row < 0))

# Retirando as linhas que contém valores negativos.
dataset <- dataset[-c(which(colunas_valores_negativos)),]
dim(dataset)
View(dataset)
summary(dataset)

# Verificando a porcentagem dos valores NA
sort(round((colSums(is.na(dataset)) * 100) / nrow(dataset),2))

# Como nas variáveis 'handwashing_facilities' e 'excess_mortality'
# a maior parte dos dados são NA, essas mesmas colunas serão removidas do dataset.

dataset$handwashing_facilities <- NULL
dataset$excess_mortality <- NULL

dim(dataset)

# Em R, é importante utilizar fator nas variáveis categóricas.
dataset$tests_units <- as.factor(dataset$tests_units)
str(dataset)

# Com os valores tratados, iremos efetuar o imputation para os valores NA

# Primeiro, precisamos identificar a correlação entre as variáveis para então criar
# as formulas preditoras.

dataset_aux <- dataset[, unlist(lapply(dataset, is.numeric))]

cor.ci(dataset_aux, method = "spearman")

predictor.selection = quickpred()

dataset_aux <- dataset
?mice
methods(mice)
imputation <- mice(dataset_aux, m = 5, maxit = 5, method = "rf")

summary(imputation)
View(imputation$imp$new_cases)


