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

# Definição ambiente de trabalho
setwd("C:/Users/guice/Desktop/ProjetoMentoriaDSA")

# Bibliotecas
install.packages("dplyr")
install.packages("tidyr")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("readr")
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(readr)
# Carregamento do dataset
?read_csv
dataset <- read_csv("owid-covid-data-26-06-2021.csv")

# ANÁLISE EXPLORATÓRIA DOS DADOS #

# Visualização do Dataset
View(dataset)

# Apresentação dos dados de medidas centrais
summary(dataset)

# Classes de variáveis
str(dataset)

# Validando total de NA nas colunas
Qtd_NA_coluna <- colSums(is.na(dataset))
View(Qtd_NA_coluna)

# Validando total de NA na coluna target
colSums(is.na(dataset[,c("reproduction_rate")]))

# Neste cenário, será retirado as observações com valores NA da coluna reproduction_rate
dataset <- dataset %>% drop_na("reproduction_rate")
View(dataset)
# Validando a remoção dos valores NA
colSums(is.na(dataset[,c("reproduction_rate")]))

# Plotando série temporal para reproduction_rate
df_america_sul <- filter(dataset, continent %in% "South America")
View(df_america_sul)

# Buscando o início e fim dos dados do dataset para plotagem
min_date <- min(df_america_sul$date)
max_date <- max(df_america_sul$date)
min_date
max_date

ggplot(data = df_america_sul, mapping = aes(x = date, y = reproduction_rate), 
      group = date,
      main = "Relação de Casos Secundários a partir de Casos Primários pelo Tempo") +
  geom_line() +
  # Separar por localidade (gráfica fica sujo)
  facet_wrap( ~ location ) +
  xlab("Data") +
  ylab("Taxa de Reprodução para Casos Secundários") + 
  xlim( as.Date(c(min_date, max_date), format="%Y-%m-%d") )

# Assim, queremos responder: Em qual data que a variável reproduction_rate será 0?