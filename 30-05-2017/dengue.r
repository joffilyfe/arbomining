# Carregando librarys
library(ggplot2)
library(zoo)
library(lubridate)
library(stringr)
library(dplyr)


# Carregando dataset
dengue_dataset <- read.csv("~/Pessoal/tcc/casos-dengue2016.csv",
                           sep = ";")

# Fatorando o dataset em 10 linhas
three_thousand <- dengue_dataset

# Filtrando linhas que possuem data real
data_clean <- three_thousand %>%
  filter(str_detect(dt_nascimento, "-"))

# Calculando a idade dos pacientes
# data_clean$idade <- as.integer((as.yearmon(today()) - as.yearmon(data_clean$dt_nascimento)))

# ggplot(data = data_clean) + geom_histogram(data = data_clean, aes(x = data_clean$idade), bins = 30)
# km <- kmeans(ten$idade, 2)
# km


com_febre <- dengue_dataset %>%
  filter(str_detect(febre, "1"))

sem_febre <- dengue_dataset %>%
  filter(str_detect(febre, "2"))


# qplot(no_bairro_residencia, data = three_thousand)
# data_clean$no_bairro_residencia

# Filtrando as tuplas que possuem o bairro preenchico
com_bairros <- data_clean %>%
  filter(!grepl("^$", no_bairro_residencia))

# Ordenando os bairros pelo número de agrupamento
bairros <- count(com_bairros, no_bairro_residencia, sort = TRUE)

# Filtrando os dez primeiros colocados
bairros <- bairros[1:10,]

# Exibindo com o ggplot
ggplot(data = bairros) + geom_point(data = bairros, aes(x = 1:10, y = n, col=bairros$no_bairro_residencia))

# Exibindo com o qplot
qplot(data = bairros, x = no_bairro_residencia, weight = n, geom = "bar", fill = no_bairro_residencia)


ggplot(data = bairros) +
  geom_bar(data = bairros,
           aes(x = reorder(no_bairro_residencia, -n),
               weight = n, fill=bairros$no_bairro_residencia))


