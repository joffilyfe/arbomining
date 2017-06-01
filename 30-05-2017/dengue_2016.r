library(ggplot2)
library(zoo)
library(lubridate)
library(stringr)
library(dplyr)


# Carregando dataset
dengue_dataset = read.csv("~/Pessoal/tcc/casos-dengue2016.csv", sep = ";")

raca_labels <- c("Branca", "Preta", "Amarela", "Parda", "Indígena")
raca <- data.frame(raca = raca_labels, tp_raca_cor = c(1, 2, 3, 4, 5))


# Usando filtros e programação funcional para contar quais raças foram informadas
quantidade_por_raca <- filter(dengue_dataset, tp_raca_cor != "") %>%
  filter(., tp_raca_cor != "9") %>%
  count(., tp_raca_cor, sort = TRUE)

# Transformando uma tabela em dataframe
quantidade_por_raca <- data.frame(quantidade_por_raca)

# Fazendo merge entre a descrição e os códigos
quantidade_por_raca <- merge(quantidade_por_raca, raca)

# Exibindo um summário
summary(quantidade_por_raca)
quantidade_por_raca

# Fazendo um plot com linha
ggplot(data = quantidade_por_raca) + 
  geom_line(data = quantidade_por_raca, aes(x = raca, y = n, col = quantidade_por_raca$raca, group = 1)) +
  xlab("Raças") +
  ylab("Quantidade")

# Fazendo plot com barras
qplot(data = quantidade_por_raca, x = raca,
      weight = n, fill = raca, geom = "bar",
      group = 1)
