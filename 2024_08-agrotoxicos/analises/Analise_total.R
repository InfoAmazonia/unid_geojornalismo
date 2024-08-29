##Paper Agrotoxicos e Neoplasias infantojuvenis - InfoAmazonia##
#Analise de Dados

setwd ("C:/Users/tatic/Dropbox/00. TRABALHANDO/08. Agrotoxicos e cancer infantojuvenil/00. Estudo_Agrotoxicos_2024/01. Banco de Dados")

# Pacotes usados
library(readr)
library(tidyverse)
library(dplyr)
library(spdep)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(scales)
library(tmap)
library(geobr)
library(broom)

##AREA TOTAL
#Total 5 / 10 / 50 por cento da area municipal

#Estimar area total em porcentagem
area_munic <- read.csv("area_munic.csv", sep = ";", dec = ".", header = TRUE)
total <- read.csv("area_plantada_total.csv", sep = ";", dec = ".", header = TRUE)
summary(total)

# Somar valor diferentes de um mesmo municipio e ano 
#(MapBiomas apresenta valor por bioma separadamente, ex. Caceres - 5102504) 
total_soma <- total %>%
  group_by(geocode) %>%
  summarise(across(starts_with("X"), sum, na.rm = TRUE))

print(total_soma)
summary(total_soma)

# Checar dados somados usando 2207603 de exemplo
cidadeteste <- subset(total_soma, geocode == 2207603, select = c(geocode, X2013, X2014, X2015))
print(cidadeteste)

# Fazendo um left join
area_total <- left_join(area_munic,total_soma, by = "geocode")

#transformar dados em colunas
area_total <- area_total %>%
  pivot_longer(cols = starts_with("X"), 
               names_to = "year", 
               values_to = "area_total")

area_total$year <- substr(area_total$year, 2, nchar(area_total$year))
summary (area_total)
print(area_total)

# Converter km2 para hectares
str(area_total)
area_total$area_munic <- as.numeric(as.character(area_total$area_munic))
area_total$area_munic_ha <- area_total$area_munic * 100
summary(area_total)

#Estimar area de algodao em porcentagem
str(area_total)

# Convertendo as colunas para numérico
area_total$area_munic_ha <- as.numeric(as.character(area_total$area_munic_ha))

# Calculando a porcentagem da área de algodao em relação à área municipal
summary(area_total)
area_total$area_total_perc <- (area_total$area_total / 
                                 area_total$area_munic_ha) * 100

#Transformar NA em zero
area_total[is.na(area_total)] <- 0

# Supondo que o dataframe se chama df e a coluna area_milho_perc existe
area_total$total_5 <- ifelse(area_total$area_total_perc > 5, 1, 0)

area_total$total_10 <- ifelse(area_total$area_total_perc > 10, 1, 0)

area_total$total_30 <- ifelse(area_total$area_total_perc > 30, 1, 0)

area_total$total_50 <- ifelse(area_total$area_total_perc > 50, 1, 0)

area_total <- filter(area_total, year != 2022)

summary(area_total)

#juntar bancos de desfechos com total
#leucemia casos
leucemia <- read.csv("leucemia_casos_tx.csv", 
                     sep = ";", dec = ",")

leucemia <- leucemia %>%
  pivot_longer(cols = starts_with("tx"), 
               names_to = "year", 
               values_to = "leuc_cases")

leucemia$year <- substr(leucemia$year, 3, nchar(leucemia$year))

banco_total <- merge(area_total, leucemia, by = c("geocode", "year"), all = TRUE)

summary (banco_total)

banco_total <- select(banco_total, - munic.y)

#leucemia obitos
leucemia_obitos <- read.csv("leucemia_obitos_tx.csv", 
                            sep = ";", dec = ",")

leucemia_obitos <- leucemia_obitos %>%
  pivot_longer(cols = starts_with("tx"), 
               names_to = "year", 
               values_to = "leuc_obitos")

leucemia_obitos$year <- substr(leucemia_obitos$year, 3, nchar(leucemia_obitos$year))

summary (leucemia_obitos)

banco_total <- merge(banco_total, leucemia_obitos, by = c("geocode6", "year"), all = TRUE)

summary (banco_total)

banco_total <- select(banco_total, - munic)

banco_total <- rename(banco_total, munic = munic.x)

#linfoma 
linfoma <- read.csv("linfoma_casos_tx.csv", 
                    sep = ";", dec = ",")

linfoma <- linfoma %>%
  pivot_longer(cols = starts_with("tx"), 
               names_to = "year", 
               values_to = "linfoma")

linfoma$year <- substr(linfoma$year, 3, nchar(linfoma$year))

summary (linfoma)

summary (banco_total)

banco_total <- merge(banco_total, linfoma, by = c("geocode", "year"), all = TRUE)

summary(banco_total)

banco_total <- rename(banco_total, munic = munic.x)

#malformações
malformacoes <- read.csv("malformacao_fetal_tx.csv", 
                         sep = ";", dec = ",")

malformacoes <- malformacoes %>%
  pivot_longer(cols = starts_with("tx"), 
               names_to = "year", 
               values_to = "malformacoes")

malformacoes$year <- substr(malformacoes$year, 3, nchar(malformacoes$year))

summary (malformacoes)

summary (banco_total)

banco_total <- rename (banco_total, geocode6 = geocode6.y)

banco_total <- merge(banco_total, malformacoes, by = c("geocode6", "year"), all = TRUE)

banco_total <- select(banco_total, - Municipio)

#óbitos fetais
obitos_fetais <- read.csv("obitos_fetais_tx.csv", 
                          sep = ";", dec = ",")

obitos_fetais <- obitos_fetais %>%
  pivot_longer(cols = starts_with("tx"), 
               names_to = "year", 
               values_to = "obitos_fetais")

obitos_fetais$year <- substr(obitos_fetais$year, 3, nchar(obitos_fetais$year))

summary (obitos_fetais)

summary (banco_total)

banco_total <- merge(banco_total, obitos_fetais, by = c("geocode6", "year"), all = TRUE)

banco_total <- select(banco_total, - geocode6.x, -munic.y, -geocode.y)

banco_total <- rename (banco_total, munic = munic.x)

banco_total <- rename (banco_total, geocode = geocode.x)

banco_total <- filter(banco_total, year != 2022)

#exportar banco de milho com desfechos em taxas por 100 mil

write.csv(banco_total, "banco_total.csv", row.names = FALSE)

# read all municipalities
banco_total <- read.csv("banco_total_final.csv", sep = ",", dec = ".", header = TRUE)

municipalities <- read_municipality(
  year = 2019,
  showProgress = FALSE)

summary (municipalities)

#merge com municipalities
colnames(municipalities)[colnames(municipalities) == "code_muni"] <- "geocode"

banco_total <- merge(banco_total, municipalities, by = "geocode", all = TRUE)

##Modelos de Associação - Risco Relativo  - TOTAL

#identificar UFs para total
summary(banco_total)

result <- banco_total %>%
  filter(year == 2021) %>%
  group_by(abbrev_state) %>%
  summarize(
    total_municipios = n(),
    municipios_com_total = sum(total_5 == 1),
    proporcao_com_total = municipios_com_total / total_municipios
  ) %>%
  filter(proporcao_com_total > 0.5)

print(result)

banco_total$malformacoes <- as.numeric(banco_total$malformacoes)
banco_total$obitos_fetais <- as.numeric(banco_total$obitos_fetais)

#Goiás - GO
banco_total_go <- banco_total %>%
  filter(abbrev_state == "GO")

#Malformações Congenitas

# Ajustando o modelo de regressão Poisson
modelo_poisson <- glm(malformacoes ~ total_5, data = banco_total_go, 
                      family = quasipoisson)

# Extraindo os resultados do modelo Poisson
resultados_poisson <- tidy(modelo_poisson, exponentiate = TRUE, conf.int = TRUE)

print(resultados_poisson)

#Óbitos Fetais

# Ajustando o modelo de regressão Poisson
modelo_poisson <- glm(obitos_fetais ~ total_50, data = banco_total_go, 
                      family = quasipoisson)

# Extraindo os resultados do modelo Poisson
resultados_poisson <- tidy(modelo_poisson, exponentiate = TRUE, conf.int = TRUE)

print(resultados_poisson)

#Mato Grosso do Sul - MS
banco_total_ms <- banco_total %>%
  filter(abbrev_state == "MS")

#Malformações Congenitas
# Ajustando o modelo de regressão Poisson
modelo_poisson <- glm(malformacoes ~ total_5, data = banco_total_ms, 
                      family = quasipoisson)

# Extraindo os resultados do modelo Poisson
resultados_poisson <- tidy(modelo_poisson, exponentiate = TRUE, conf.int = TRUE)

print(resultados_poisson)

#Óbitos Fetais
# Ajustando o modelo de regressão Poisson
modelo_poisson <- glm(obitos_fetais ~ total_30, data = banco_total_ms, 
                      family = quasipoisson)

# Extraindo os resultados do modelo Poisson
resultados_poisson <- tidy(modelo_poisson, exponentiate = TRUE, conf.int = TRUE)

print(resultados_poisson)

#Mato Grosso - MT
banco_total_mt <- banco_total %>%
  filter(abbrev_state == "MT")

#Malformações Congenitas
# Ajustando o modelo de regressão Poisson
modelo_poisson <- glm(malformacoes ~ total_5, data = banco_total_mt, 
                      family = quasipoisson)

# Extraindo os resultados do modelo Poisson
resultados_poisson <- tidy(modelo_poisson, exponentiate = TRUE, conf.int = TRUE)

print(resultados_poisson)

#Óbitos Fetais
# Ajustando o modelo de regressão Poisson
modelo_poisson <- glm(obitos_fetais ~ total_5, data = banco_total_mt, 
                      family = quasipoisson)

# Extraindo os resultados do modelo Poisson
resultados_poisson <- tidy(modelo_poisson, exponentiate = TRUE, conf.int = TRUE)

print(resultados_poisson)

#Paraná - PR
banco_total_pr <- banco_total %>%
  filter(abbrev_state == "PR")

#Malformações Congenitas
# Ajustando o modelo de regressão Poisson
modelo_poisson <- glm(malformacoes ~ total_5, data = banco_total_pr, 
                      family = quasipoisson)

# Extraindo os resultados do modelo Poisson
resultados_poisson <- tidy(modelo_poisson, exponentiate = TRUE, conf.int = TRUE)

print(resultados_poisson)

#Óbitos Fetais
# Ajustando o modelo de regressão Poisson
modelo_poisson <- glm(obitos_fetais ~ total_5, data = banco_total_pr, 
                      family = quasipoisson)

# Extraindo os resultados do modelo Poisson
resultados_poisson <- tidy(modelo_poisson, exponentiate = TRUE, conf.int = TRUE)

print(resultados_poisson)

#Rio Grande do Sul - RS
banco_total_rs <- banco_total %>%
  filter(abbrev_state == "RS")

#Malformações Congenitas
# Ajustando o modelo de regressão Poisson
modelo_poisson <- glm(malformacoes ~ total_50, data = banco_total_rs, 
                      family = quasipoisson)

# Extraindo os resultados do modelo Poisson
resultados_poisson <- tidy(modelo_poisson, exponentiate = TRUE, conf.int = TRUE)

print(resultados_poisson)

#Óbitos Fetais
# Ajustando o modelo de regressão Poisson
modelo_poisson <- glm(obitos_fetais ~ total_30, data = banco_total_rs, 
                      family = quasipoisson)

# Extraindo os resultados do modelo Poisson
resultados_poisson <- tidy(modelo_poisson, exponentiate = TRUE, conf.int = TRUE)

print(resultados_poisson)

#Santa Catarina - SC
banco_total_sc <- banco_total %>%
  filter(abbrev_state == "SC")

#Malformações Congenitas
# Ajustando o modelo de regressão Poisson
modelo_poisson <- glm(malformacoes ~ total_30, data = banco_total_sc, 
                      family = quasipoisson)

# Extraindo os resultados do modelo Poisson
resultados_poisson <- tidy(modelo_poisson, exponentiate = TRUE, conf.int = TRUE)

print(resultados_poisson)

#Óbitos Fetais
# Ajustando o modelo de regressão Poisson
modelo_poisson <- glm(obitos_fetais ~ total_30, data = banco_total_sc, 
                      family = quasipoisson)

# Extraindo os resultados do modelo Poisson
resultados_poisson <- tidy(modelo_poisson, exponentiate = TRUE, conf.int = TRUE)

print(resultados_poisson)

#São Paulo - SP
banco_total_sp <- banco_total %>%
  filter(abbrev_state == "SP")

#Malformações Congenitas
# Ajustando o modelo de regressão Poisson
modelo_poisson <- glm(malformacoes ~ total_30, data = banco_total_sp, 
                      family = quasipoisson)

# Extraindo os resultados do modelo Poisson
resultados_poisson <- tidy(modelo_poisson, exponentiate = TRUE, conf.int = TRUE)

print(resultados_poisson)

#Óbitos Fetais
# Ajustando o modelo de regressão Poisson
modelo_poisson <- glm(obitos_fetais ~ total_5, data = banco_total_sp, 
                      family = quasipoisson)

# Extraindo os resultados do modelo Poisson
resultados_poisson <- tidy(modelo_poisson, exponentiate = TRUE, conf.int = TRUE)

print(resultados_poisson)