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

#### DAQUI PARA BAIXO DESCONSIDERAR ####

# Verificando correlação entre as variáveis com valores NA

dataset_aux <- dataset[, unlist(lapply(dataset, is.numeric))]
datacor <- cor(dataset_aux, use = "pairwise.complete.obs")
corr_var(dataset_aux,
        reproduction_rate, 
        top = 43
)

# Retirando os valores NA e verificando a correlação
dataset_sem_na <- dataset %>% filter(complete.cases(.))
View(dataset_sem_na)

dataset_aux <- dataset_sem_na[, unlist(lapply(dataset_sem_na,  is.numeric))]
datacor <- cor(dataset_aux)
corr_var(dataset_aux,
         reproduction_rate,
         top = 43)
                  

## Iremos efetuar a trativa dos valores NA e refazer o teste de correlação.
## SUBSTITUÍNDO VALORES NA UTILIZANDO MICE ##







# Vamos remover as variáveis que não serão utilizadas
variaveis_dispensadas <- c("iso_code", "continent", "life_expectancy", "hospital_beds_per_thousand",
                           "diabetes_prevalence", "female_smokers", "male_smokers", "cardiovasc_death_rate",
                           "aged_70_older", "aged_65_older", "new_vaccinations_smoothed_per_million", "weekly_hosp_admissions_per_million",
                           "weekly_hosp_admissions", "weekly_icu_admissions_per_million", "icu_patients_per_million", "hosp_patients_per_million")
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



##           ##
##           ##
#### PLOTs ####
##           ##
##           ##

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




##                                       ##
##                                       ##
#### DEFINIÇÃO DE VARIÁVEIS PREDITORAS ####
##                                       ##
##                                       ##

# Primeira opção seria preencher os valores com a média das colunas (Acredito que um dos problemas aqui seria gerar outliers);
# Segunda opção seria gerar modelo de regressão para esses valores.



