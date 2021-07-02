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
dataset <- read_csv("dataset_inicio_vacina.csv")


View(dataset)
dim(dataset)
str(dataset)

# Validar se existe registros duplicados da mesma localidade e dia.

registro_duplicado <- dataset %>%
                       group_by(date, location)
dim(registro_duplicado)

# Validando NAs

colSums(is.na(dataset))

# Validando variável tests_units: Seria as unidades utilizadas para reportar os testes.
ggplot(data = dataset) +
  geom_bar(mapping = aes(x = tests_units, fill = tests_units)) +
  theme_minimal()

# Verificando as localidades
table(dataset$location)

# Conforme lista, existe uma localidade chamada World, onde subentendesse que
# seria as informações mundiais;
# Será que vale a pena manter?

ggplot(dataset, aes(x=location, y=total_cases)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_minimal()
# Com o gráfico, da pra identificar que a localidade world tem muito mais casos que as demais. 
# Vamos confirmar tirando diferença entre o que é 'World' e o que não é.

df_total_cases = data.frame(x = c("World", "Demais Localidades"),
                            y = c(sum(dataset[!dataset$location == "World",]$total_cases), 
                                  sum(dataset[!dataset$location != "World",]$total_cases)))
View(df_total_cases)

ggplot(df_total_cases, aes(x=x, y=y)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_minimal()

# Com isso, a localidade world será retirada do dataset

rows_to_delete <- which(dataset$location == 'World')
# Confirmando os indexs
View(dataset[rows_to_delete,])
dataset = dataset[-c(rows_to_delete),]
dim(dataset)

# Vamos confirmar se foram excluídas as localidades == 'World'
dataset[dataset$location == 'World',]
# Como podemos ver, a localidade foi deletada.

write_csv(dataset, "dataset_sem_localidade_mundial.csv")



#### DESCONSIDERAR ####


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



