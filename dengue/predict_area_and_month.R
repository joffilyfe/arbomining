library(tidyverse)
library(lubridate)

set.seed(1000)

# Load dataset
dengue_2015_pb <- read_csv("/Users/joffily/IFPB/TCC/secretaria/data/dengue-2015.csv", col_names=TRUE)

summary(dengue_2015_pb)

# Extraindo colunas interessadas
casos_dengue_2015 <- dengue_2015_pb %>% select(DT_NOTIFIC, ID_MUNICIP)

# Convertendo datas
casos_dengue_2015$DT_NOTIFIC <- ymd(casos_dengue_2015$DT_NOTIFIC)

# Vendo resultado
head(casos_dengue_2015)

# Guardando os meses
casos_dengue_2015$month <- month(casos_dengue_2015$DT_NOTIFIC)

# Selecionando o mes e o municipio
dados <- casos_dengue_2015 %>% select(month, ID_MUNICIP)

# Contando quantos casos por mês
casos_por_mes <- count(dados, month)

# Plotando os casos
ggplot(data = casos_por_mes) +
  geom_point(data = casos_por_mes, aes(x = month, y = n)) +
  geom_line(data = casos_por_mes, aes(x = month, y = n)) +
  theme_bw(12)
