# PROJETO MENTORIA DSA - 2021 #
#
# PROBLEMA:  Prever quando um país estará com a pandemia sob controle.
#
# Pontos a serem explorados:
#  - Qual métrica será utilizada para definir com um país conquistou o nível de segurança.
# 
# Dataset desenvolvido a partir de informações de: https://github.com/CSSEGISandData/COVID-19
#
#
# Conforme análise das variáveis do dataset, temos como variável target o reproduction_rate
# reproduction_rate: Real-time estimate of the effective reproduction rate (R) of COVID-19
# Essa variável representa a quantidade de casos de COVID secundários afetados pelo caso primário, ou seja,
# com um exemplo hipotético temos que uma pessoa da KOREA, localizada no Sul da KOREA
# infectada pelo vírus, pode espalhar o vírus para outras 5 pessoas.

# Script responsável por efetuar um subset do dataset completo.

# Definição ambiente de trabalho
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
library(lares)
library(mice)
library(tidyverse)
library(dplyr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(readr)
# Carregamento do dataset
?read_csv
dataset <- read_csv("owid-covid-data-26-06-2021.csv")

########################################
##                                    ##
#### ANÁLISE EXPLORATÓRIA DOS DADOS ####
##                                    ##
########################################

# Visualização do Dataset
View(dataset)

# Classes de variáveis
str(dataset)


##                             ##
##                             ##
#### TRANSFORMAÇÃO DOS DADOS ####
##                             ##
##                             ##


# Validando total de NA nas colunas
Qtd_NA_coluna <- colSums(is.na(dataset))
View(Qtd_NA_coluna)
dim(dataset)

# De acordo com a visualização de NA, existe algumas colunas onde todas as observações
# são NA, logo, iremos remover essas colunas
dataset <- dataset[,c(!Qtd_NA_coluna == nrow(dataset))]
View(dataset)
str(dataset)
dim(dataset)

# Selecionando as observações com NA
dataset %>%
  filter(!complete.cases(.)) %>%
  View()
# Como podemos ver, ainda em todas observações, ao menos uma variável possue valor NA.

# Validando total de NA na coluna target
colSums(is.na(dataset[,c("reproduction_rate", "date")]))

# Neste cenário, será retirado as observações com valores NA da coluna reproduction_rate
dataset <- dataset %>% drop_na("reproduction_rate")
View(dataset)
dim(dataset)

# Validando a remoção dos valores NA
colSums(is.na(dataset[,c("reproduction_rate")]))

# Validando a variável qualitativa nominal: tests_units
sum(is.na(dataset$tests_units))
table(dataset$tests_units)

# Vamos alterar os valores de NA para: no information
dataset <- dataset %>%
          mutate(tests_units = if_else(is.na(tests_units), 
                                       'no information', 
                                       tests_units))

# TODO: Alterar para factor
table(dataset$tests_units)
class(dataset$tests_units)

# Validando total de NA nas colunas novamente
Qtd_NA_coluna <- colSums(is.na(dataset))
View(Qtd_NA_coluna)


# Nosso dataset possui 78887 observações. 
# Algumas variáveis ainda possuem um alto índice de valores NA
# Algumas referências citam que observações com mais de 70% de Na é aceitável o drop da mesma.
# Referência: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3701793/
# Listwise vs. Pairwise = Quando os dados Na forem completamente randons (MCAR)

missing_data_rate <- ((Qtd_NA_coluna * 100) / nrow(dataset) > 75)
# Variáveis com mais de 75% de NA
# total_vaccinations
# new_vaccinations
# people_vaccinated
# people_fully_vaccinated 
# total_vaccinations_per_hundred
# people_fully_vaccinated_per_hundred
# excess_mortality 
# people_vaccinated_per_hundred

# Visualização das colunas com + de 75 % de NA
dataset_missing_rate <- dataset[, missing_data_rate]
View(dataset_missing_rate)
summary(dataset_missing_rate)

# Essas colunas fazem sentido possuírem uma grande quantidade de valores NA
# visto que a vacina só foi disponibilizada praticamente 1 ano depois da doença;
# Como nosso problema de negócio está diretamente ligado a vacina, os dados anteriores a vacina
# serão dropados.

# Verificando inicio da vacinação
dataset_inicio_vacina <- dataset %>%
                          filter(total_vaccinations >= 1) %>%
                          group_by(date) %>%
                          arrange(total_vaccinations)

dim(dataset_inicio_vacina)
colSums(is.na(dataset_inicio_vacina))

# Iremos manter apenas a variável Location
dataset_inicio_vacina$iso_code <- NULL
dataset_inicio_vacina$continent <- NULL

# Dataset para trabalhar
View(dataset_inicio_vacina)

write_csv(dataset_inicio_vacina, "dataset_inicio_vacina.csv")
