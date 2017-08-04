library(tidyverse)
library(lubridate)
library(stringr)

set.seed(1000)


# Funções para plotar casos por cidade
ver_casos_em <- function(id) {
  cidade <- casos_dengue_2016 %>%
    filter(ID_MUNICIP == id) %>%
    count(month) %>%
    merge(meses)
  return(cidade)
}

plotar_casos <- function(cidade) {
  cidade %>% ggplot() +
    geom_point(data=cidade, aes(x=reorder(cidade$nome, month), y = n, size = 5, colour = n)) +
    geom_line(data = cidade, aes(x = month, y = n), linetype = 2) +
    theme_bw(12) +
    ylab("Casos em..") +
    xlab("Meses") +
    xlab("Meses") + scale_colour_gradient(high = "red", low = "#cccccc")
}


# Load dataset
dengue_2016_pb <- read_csv("/Users/joffily/IFPB/TCC/secretaria/data/dengue-2016.csv", col_names=TRUE)

# Seleciona dados interessantes para realizar o trabalho
casos_dengue_2016 <- dengue_2016_pb %>%
 dplyr::select(DT_NOTIFIC, ID_MUNICIP)

# Faz o parser da datas
casos_dengue_2016$DT_NOTIFIC <- ymd(casos_dengue_2016$DT_NOTIFIC)

# Filtra os casos apenas por datas válidas
casos_dengue_2016 <- casos_dengue_2016 %>%
  filter(str_detect(DT_NOTIFIC, "-"))

# Salva dados limpos em arquivo
# write_csv(count(casos_dengue_2016, ID_MUNICIP, sort = TRUE), "./numero_casos_2016.csv")

# Guardando os meses
casos_dengue_2016$month <- month(casos_dengue_2016$DT_NOTIFIC)

# Selecionando o mes e o municipio
dados2 <- casos_dengue_2016 %>% dplyr::select(month, ID_MUNICIP, DT_NOTIFIC)

# Contando por municipio
count(dados2, ID_MUNICIP, sort = TRUE)
# Contando quantos casos por mês
casos_por_mes2 <- count(dados2, month)

# Atribuindo meses ao dataframe
meses <- data.frame(nome=c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN", "JUL", "AGO", "SEP", "OUT", "NOV", "DEZ"), month=c(1:12))
casos_por_mes2 <- merge(casos_por_mes2, meses)

# Plotando
ggplot(data = casos_por_mes2) +
  geom_point(data = casos_por_mes2, aes(x = reorder(nome, month), y = n)) +
  geom_line(data = casos_por_mes2, aes(x = month, y = n)) +
  xlab("Meses") +
  ylab("Quantidade de casos") +
  theme_bw(12)


# Criando um modelo para a previsão de notificações por mês
# e quantidade de precipitação de chuvas
casos_lm <- lm(casos_por_mes2$n ~ c(1:12))

# Usando a função predict para gerar uma predição futura
casos_pred <- predict(casos_lm, data.frame(mes=c(1:12)),
        interval = c("none", "confidence", "prediction"),
        level = 0.95, type = c("response", "terms"),
        terms = NULL, na.action = na.pass,
        pred.var = res.var/weights, weights = 1)

ggplot(data = data.frame(n = casos_pred, mes = c(1:12)), aes(x = mes, y = n) ) +
  geom_line() +
  scale_x_continuous(breaks=c(1:12)) +
  geom_point() +
  geom_line(data = casos_por_mes2, aes( x = c(1:12), y = n), colour = "blue") +
  theme_bw() +
  ylab("Quantidade de notificações x Previsão") +
  xlab("Mês")



# Criando modelo de dados para a cidade de joão pessoa

jp <- casos_dengue_2016 %>%
  filter(ID_MUNICIP == 250750) %>%
  count(month) %>%
  merge(meses)


# Cortando o modelo de dados até o mês de Junho
jp_ate_junho <- jp[1:6,]

# Carregando a quantidade de precipitação em mm para João pessoa até o mês de Junho
precipitacao_jp_2016 <- c(103, 127, 140, 292, 359, 177)

# Criando modelo de previsão
cortado_lm <- lm(n ~ month + precipitacao_jp_2016, jp_ate_junho)
jp_ate_junho$caso_previstos <- predict(cortado_lm, jp_ate_junho)


# Cores para o nosso gráfico
cores <- c("Previsto"="blue", "Real"="black")

ggplot() +
  geom_point(data=jp_ate_junho, aes(x=reorder(cortado$nome, month), y = caso_previstos)) +
  geom_line(data=jp_ate_junho, aes(x = month, y = caso_previstos, group = 1, colour = "Previsto")) +
  geom_point(data=jp_ate_junho, aes(x = month, y = n)) +
  geom_line(data=jp_ate_junho, aes(x = month, y = n, colour = "Real")) +
  theme_bw(12) +
  ylab("Quantidade de notificações") +
  xlab("Mês") +
  scale_colour_manual(name="Quantidade de casos x Previsão",values=cores) +
  ggtitle("Dengue no ano de 2016")

