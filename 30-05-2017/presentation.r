# Carregando a library de gráficos bonitinhos
library(ggplot2)

# Definindo o número de random()
set.seed(1)

# Carregamos o dataset sobre as flores Iris
data(iris)

# Extraimos as colunas de 1 a 4 para a variavel my_iris
my_iris <- iris[,1:4]

# Extraimos a coluna 5 para a variável especies
species <- iris[,5]

# Criamos um agrupamento de 3
km <- kmeans(my_iris, 3, nstart = 20)

# Imprimindo o conteúdo de KM
# Aqui nós conseguimos dados com o total_SS
# O total_SS significa o sum of squares by cluster
# com o dataset do iris nós chegamos a 88,4% de confiabilidade
km

# Exibindo o resultado do agrupamento em forma de tablea
table(km$cluster, species)

# Exindo o resultado via gráfico
ggplot(iris, aes(Petal.Length, Petal.Width, color = species)) + geom_point()
