# Carregando o pacote library
#install.packages("class")
#install.packages("gmodels")
#install.packages("dplyr")
#install.packages("ggplot2")
library(class)
library(ggplot2)
library(dplyr)
library(gmodels)

dados <- read.csv("http://datascienceacademy.com.br/blog/aluno/RFundamentos/Datasets/ML/bc_data.csv",
                  stringsAsFactors = FALSE)
dados=dados[-1]

## Olhando para estrutura dos dados
str(dados)
##Verificando se existem valores NA

any(is.na(dados))

## Convertendo a variável diagnosis em fator

dados$diagnosis <- factor(dados$diagnosis, levels = c("B", "M"), labels = c("Benigno", "Maligno"))

str(dados$diagnosis)

# Verificando a proporção
round(prop.table(table(dados$diagnosis)) * 100, digits = 1)

# Testando a função de normalização - os resultados devem ser idênticos
normalizar(c(1, 2, 3, 4, 5))

summary(dados[c("radius_mean", "area_mean", "smoothness_mean")])

dados_norm <- as.data.frame(lapply(dados[2:31], normalizar))



# Criando dados de treino e dados de teste
dados_treino <- dados_norm[1:469, ]
dados_teste <- dados_norm[470:569, ]
# Criando os labels para os dados de treino e de teste
dados_treino_labels <- dados[1:469, 1]
dados_teste_labels <- dados[470:569, 1]

#Utilizano kNN para classificação 
# Criando o modelo
modelo <- knn(train = dados_treino, test = dados_teste, cl = dados_treino_labels, k = 21)

# Carregando o gmodels


CrossTable(x = dados_teste_labels, y = modelo, prop.chisq = FALSE)


## Etapa 5: Otimizando a perfomance do modelo
# Usando a função scale() para padronizar o z-score
dados_z <- as.data.frame(scale(dados[-1]))
# Confirmando transformação realizada com sucesso

ummary(dados_z$area_mean)

# Criando novos datasets de treino e de teste
dados_treino <- dados_z[1:469, ]
dados_teste <- dados_z[470:569, ]
dados_treino_labels <- dados[ 1: 469, 1]
dados_teste_labels <- dados[ 470: 569, 1]


## calculamos o modelo para k que minimiza o erro

## Calculando a taxa de erro
prev = NULL
taxa_erro = NULL
suppressWarnings(
  for(i in 1:20){
    set.seed(101)
    prev = knn(train = dados_treino, test = dados_teste, cl = dados_treino_labels, k = i)
    taxa_erro[i] = mean(dados$diagnosis != prev)
  })
# Obtendo os valores de k e das taxas de erro

k.values <- 1:20
df_erro <- data.frame(taxa_erro, k.values)

## Fazendo o gráfico de linha para os erros

ggplot(df_erro, aes(x = k.values, y = taxa_erro)) + geom_point()+ geom_line(lty = "dotted", color = 'red')

#Ordenando os dados pela menor taxa de erro 

menor= df_erro %>%arrange(taxa_erro)


kmin=menor$k.values[1]


# Reclassificando
modelo_v2 <- knn(train = dados_treino, test = dados_teste, cl = dados_treino_labels, k = kmin)
# Criando a cross table para comparar dados previstos com os dados reais
CrossTable(x = dados_teste_labels, y = modelo_v2, prop.chisq = FALSE)
