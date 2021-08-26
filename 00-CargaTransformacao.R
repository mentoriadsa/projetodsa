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
# Script responsável pela carga e transformação dos dados.

# Definição ambiente de trabalho
setwd("C:/Users/guice/Desktop/ProjetoMentoriaDSA")

#### Codebook das variáveis ####
# A descrição das variáveis está no arquivo codebook-variaveis.csv, dentro do repositório #

install.packages("naniar")
#### Packages utilizados ####
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(mice)
library(naniar)

# Carregamento do dataset
dataset <- read_csv("dataset/owid-covid-data-26-06-2021.csv")
View(dataset)
str(dataset)

# Validando total de NA nas colunas
Qtd_NA_coluna <- colSums(is.na(dataset))
View(Qtd_NA_coluna)

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
# Como podemos ver, ainda, em todas observações, ao menos uma variável possue valor NA.

# Após uma validação nas variáveis e no codebook, foi identificado que a variável que
# queremos prever é: reproduction_rate e date

# Então, vamos validar total de NA na coluna target
colSums(is.na(dataset[,c("reproduction_rate", "date")]))

# O dataset possui uma variável categórica: tests_units.
# Algumas tratativas serão aplicadas na variável.
sum(is.na(dataset$tests_units))
table(dataset$tests_units)

# Vamos alterar os valores de NA para: no information
dataset <- dataset %>%
  mutate(tests_units = if_else(is.na(tests_units), 
                               'no information', 
                               tests_units))

# E por fim, transformá-la em fator
dataset$tests_units <- as.factor(dataset$tests_units)
dim(dataset)

# Nosso dataset possui 98229 observações. 
# Algumas variáveis ainda possuem um alto índice de valores NA
# Algumas referências citam que observações com mais de 70% de Na é aceitável o drop da mesma.
# Referência: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3701793/
Qtd_NA_coluna <- colSums(is.na(dataset))
missing_data_rate <- ((Qtd_NA_coluna * 100) / nrow(dataset) > 75)

summary(dataset[, missing_data_rate])
View(dataset[, missing_data_rate])

# Variáveis com mais de 75% de NA
# total_vaccinations
# new_vaccinations
# people_vaccinated
# people_fully_vaccinated 
# total_vaccinations_per_hundred
# people_fully_vaccinated_per_hundred
# excess_mortality 
# people_vaccinated_per_hundred

# Essas colunas fazem sentido possuírem uma grande quantidade de valores NA
# visto que a vacina só foi disponibilizada praticamente 1 ano depois da doença;

# Iremos manter apenas a variável Location
dataset$iso_code <- NULL
dataset$continent <- NULL

# Verificando inicio da vacinação
dataset_inicio_vacina <- dataset %>%
                          filter(total_vaccinations >= 1) %>%
                          group_by(date, location) %>%
                          arrange(total_vaccinations)



View(dataset_inicio_vacina)
dim(dataset_inicio_vacina)
colSums(is.na(dataset_inicio_vacina))

# Removendo linhas sem casos da doença
dataset_inicio_vacina <- dataset_inicio_vacina[-c(which(is.na(dataset_inicio_vacina$total_cases))),]

# Validar se existe registros duplicados da mesma localidade e dia.

dim(dataset_inicio_vacina %>% group_by(date, location))

# Verificando as localidades
table(dataset$location)

# Conforme lista, existe uma localidade denominada 'World', onde subentendesse que
# seria as informações mundiais: Será que vale a pena manter?

ggplot(dataset_inicio_vacina, aes(x=location, y=total_cases)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_minimal()

# Com o gráfico, da pra identificar que a localidade 'world' tem muito mais casos que as demais. 
# Com isso podemos concluir que a localidade 'World' seria um totalizador das informações.
# Vamos confirmar tirando diferença entre o que é 'World' e o que não é.
sum(dataset_inicio_vacina[dataset_inicio_vacina$location == "World",]$total_cases)
df_total_cases = data.frame(x = c("World", "Demais Localidades"),
                            y = c(sum(dataset_inicio_vacina[!dataset_inicio_vacina$location == "World",]$total_cases), 
                                  sum(dataset_inicio_vacina[!dataset_inicio_vacina$location != "World",]$total_cases)))

ggplot(df_total_cases, aes(x=x, y=y)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_minimal()

rows_to_delete <- which(dataset_inicio_vacina$location == 'World')
dataset = dataset_inicio_vacina[-c(rows_to_delete),]
dim(dataset)

# Validando as medidas centrais do dataset
summary(dataset)
# Conforme análise do summary, existem valores negativos no dataset
# como não sabemos a razão, esses mesmos valores serão removidos.

colunas_numericas <- unlist(lapply(dataset, is.numeric))
colunas_valores_negativos <- apply(dataset[,colunas_numericas], 1, function(row) any(row < 0))

# Retirando as linhas que contém valores negativos.
dataset <- dataset[-c(which(colunas_valores_negativos)),]
dim(dataset)
summary(dataset)
View(dataset)

# Verificando a porcentagem dos valores NA
sort(round((colSums(is.na(dataset)) * 100) / nrow(dataset),2))

# Como nas variáveis 'handwashing_facilities' e 'excess_mortality'
# a maior parte dos dados são NA, essas mesmas colunas serão removidas do dataset.
dataset$handwashing_facilities <- NULL
dataset$excess_mortality <- NULL

# Também, para essa análise, iremos remover algumas colunas que 'aparentemente' irrelevantes.
dataset$aged_65_older <- NULL
dataset$aged_70_older <- NULL
dataset$female_smokers <- NULL
dataset$male_smokers <- NULL
dataset$life_expectancy <- NULL
dataset$hospital_beds_per_thousand <- NULL

dim(dataset)
str(dataset)
View(dataset)

# Validando a variável target
unique(dataset$reproduction_rate)
sum(is.na(dataset$reproduction_rate))

# Podemos ver que existem vários valores NA e vários valores únicos.
# Primeiramente vamos validar os valores NA

#### Compreensão dos Valores NA: MAR, MCAR, MNAR ####
# Antes de efetuar a imputação dos valores, iremos tentar compreender a natureza
# desses valores NAs

# Vamos validar os valores NAs por location
nas_por_location <-  dataset %>%
                        group_by(location) %>%
                        count(logical = is.na(reproduction_rate))

View(nas_por_location)
# Podemos ver que em alguns países existe um alto índice de valores NA para variável
# target. Vamos validar e separar esses países para talvez utilizá-los como dados de teste.
?order_by
paises <- sort(unique(dataset$location))


busca_paises_remover <- function (paises, valores_na) {
  paises_remover <- vector()
  for (pais in paises) {
    localidade <- valores_na[valores_na$location == pais,]
    qtd_na <- filter(localidade, localidade$logical == T)$n
    qtd_reais <- filter(localidade, localidade$logical == F)$n
    
    print(pais)
    print(length(qtd_na))
    print(length(qtd_reais))
    print('---------------------------------------')
    if (length(qtd_na) == 0) {
      next
    }    
    
    if (length(qtd_reais) == 0) {
      paises_remover <- append(paises_remover, pais)
      next
    }
    
    qtd_registros <- qtd_na + qtd_reais
    
    # Seta pais para remover se tiver mais da metade dos valores NA
    if ( qtd_na >= (qtd_registros * .5) ) {
      paises_remover <- append(paises_remover, pais)
    }
  }
  
  return(paises_remover)
}
         
# Lista de paises que iremos desconsiderar da análise por possuir poucos valores
# da variável target ou até mesmo nenhum valor
paises_remover <- busca_paises_remover(paises, nas_por_location)

dataset_paises_alto_grau_na <- filter(dataset, location %in% paises_remover) %>% arrange(location)

# Guardar os dados em um csv para posteriormente utilizá-lo como dados para teste.
write_csv(dataset_paises_alto_grau_na, "paises_com_alto_nivel_na.csv", na = "")

# Removendo os paises do dataset principal
dataset <- dataset %>%
              filter(!location %in% paises_remover) %>%
              arrange(location)

# Validando os valores nulos para idade média
dataset %>%
  filter(is.na(median_age)) %>%
  group_by(location) %>%
  summarise(n = n()) %>%
  View()

# Neste caso, vamos efetuar imputation manual, pesquisando a idade média
dataset[dataset$location == "Andorra",]$median_age <- 47
dataset[dataset$location == "Dominica",]$median_age <- 36
dataset[dataset$location == "Kosovo",]$median_age <- 31
dataset[dataset$location == "Liechtenstein",]$median_age <- 44
dataset[dataset$location == "Monaco",]$median_age <- 56
dataset[dataset$location == "San Marino",]$median_age <- 46


colSums(is.na(dataset))
dataset %>%
  filter(is.na()) %>%
  View()

# Validando países com taxas zeradas
dataset %>%
  group_by(location) %>%
  summarise(n = sum(reproduction_rate)) %>%
  View()

# Vamos buscar agora apenas os resultados sem valores  NA
dataset <- dataset[which(complete.cases(dataset)),]
dim(dataset)

dataset %>%
  group_by(location) %>%
  summarise(n = n()) %>%
  View()

write_csv(dataset, "dados_covid_sem_na.csv")

# Série temporal da taxa de reprodução do vírus
loc.plot <- function(location){
  ggplot(dataset[dataset$location == location, ], aes(x = date, y = reproduction_rate)) + 
    geom_line() +
    ylab("Taxa de Reprodução do Vírus") +
    labs(title = paste("Proliferação do Vírus ", as.character(location), sep = "")) +
    theme(text = element_text(size = 20))
}
locations <- unique(dataset$location)
lapply(locations, loc.plot)


