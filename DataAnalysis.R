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

##                                    ##
##                                    ##
#### ANÁLISE EXPLORATÓRIA DOS DADOS ####
##                                    ##
##                                    ##

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

##           ##
##           ##
#### PLOTs ####
##           ##
##           ##

ggplot(data = df_america_sul, mapping = aes(x = date, y = reproduction_rate), 
      group = date,
      main = "Relação de Casos Secundários a partir de Casos Primários pelo Tempo") +
  geom_line() +
  # Separar por localidade (gráfica fica sujo)
  facet_wrap( ~ location ) +
  xlab("Data") +
  ylab("Taxa de Reprodução para Casos Secundários") + 
  xlim( as.Date(c(min_date, max_date), format="%Y-%m-%d") )

dataset_novos_casos_data <- dataset %>% 
                            select(date, new_cases) %>%
                            group_by(date) %>%
                            summarise(new_cases = sum(new_cases, na.rm = TRUE))

dataset_mortes_casos_data <- dataset %>% 
                              select(date, new_deaths) %>%
                              group_by(date) %>%
                              summarise(new_deaths = sum(new_deaths, na.rm = TRUE)) 


dataset_reproduction_rate <- dataset %>% 
                            select(date, reproduction_rate) %>%
                            group_by(date) %>%
                            summarise(reproduction_rate = sum(reproduction_rate, na.rm = TRUE)) 

View(dataset_novos_casos_data)
View(dataset_mortes_casos_data)
View(dataset_reproduction_rate)

# Ajuste de bug do ggplot
install.packages("patchwork")
library(patchwork)

# Visualização da curva de novos casos com novas mortes
ggplot() + 
  geom_point(mapping = aes(dataset_novos_casos_data$date, dataset_novos_casos_data$new_cases, col="red")) + 
  geom_line(mapping = aes(dataset_mortes_casos_data$date, dataset_mortes_casos_data$new_deaths, col="blue")) +
  xlab("Data") + 
  ylab("Total de Afetados") +
  scale_colour_identity(guide="legend",breaks=c("red", "blue"), labels = c("Novos Casos", "Novas Mortes"))
  

# Curva de casos secundários global
ggplot() + 
  geom_line(mapping = aes(dataset_reproduction_rate$date, dataset_reproduction_rate$reproduction_rate)) +
  xlab("Data") + 
  ylab("Taxa de Contaminação")

# Validando as medidas centrais da taxa de reprodução
ggplot() + 
  geom_boxplot(mapping = aes(dataset$reproduction_rate))

ggplot() + 
  geom_boxplot(mapping = aes(dataset$new_cases))
# Conforme boxplot, dataset contém algumas valores negativos e deverão ser tratados.


# Assim, queremos responder: Em qual data que a variável reproduction_rate será 0?
# Neste cenário, teremos duas variáveis target, a data (date) e a taxa de reprodução (reproduction_rate). 


##                             ##
##                             ##
#### TRANSFORMAÇÃO DOS DADOS ####
##                             ##
##                             ##

# Vamos remover as variáveis que não serão utilizadas
variaveis_dispensadas <- c("iso_code", "continent", "life_expectancy", "hospital_beds_per_thousand",
                           "diabetes_prevalence", "female_smokers", "male_smokers", "cardiovasc_death_rate",
                           "aged_70_older", "aged_65_older", "new_vaccinations_smoothed_per_million", "weekly_hosp_admissions_per_million",
                           "weekly_hosp_admissions", "weekly_icu_admissions_per_million", "icu_patients", 
                           "icu_patients_per_million", "hosp_patients_per_million")
dataset[,variaveis_dispensadas] <- NULL

# Validando novamente os valores NA
colSums(is.na(dataset))

# Retirando valores menores que 0
dataset_non_negative_values = dataset %>% filter(new_cases >= 0, 
                                                new_cases_smoothed >= 0, 
                                                new_deaths >= 0,  
                                                new_deaths_smoothed >= 0,
                                                new_tests >= 0)



# Tratando valores NA
# Para esse cenário, não podemos remover os valores NA por completo pois senão todo dataset será removido.
# Conforme podemos verificar abaixo.
dataset_sem_na <- na.omit(dataset)
View(dataset_sem_na)

##                                       ##
##                                       ##
#### DEFINIÇÃO DE VARIÁVEIS PREDITORAS ####
##                                       ##
##                                       ##

# Primeira opção seria preencher os valores com a média das colunas (Acredito que um dos problemas aqui seria gerar outliers);
# Segunda opção seria gerar modelo de regressão para esses valores.



