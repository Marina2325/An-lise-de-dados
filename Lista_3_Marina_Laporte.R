# Lista 3 - Análise de dados (Marina laporte Cotias)

## Questão 1: https://github.com/Marina2325/Analise-de-Dados-2019 

## Questão 2 

x <- 2 

y <- 4 

z <- x + y

z*11222936402
#Resposta: 67337618412

## Questão 3 

install.packages("ffbase", dependencies = TRUE)
install.packages("ff")

library("ffbase")
library("ff")

#Descrição dos tipos de variáveis 

str(mtcars)

#Número de dimensões da base

dim(mtcars)

#Imprimir a terceira coluna da base

mtcars[,3]

#Imprimir a segunda linha da base

mtcars[2,]

#O quarto elemento da variável "cyl"

mtcars$cyl[4]

#Resumo descritivo 

summary(mtcars)

## Questão 4 

# modificar o diretório (caminho da pasta onde tão os arquivos)

setwd("C:/Users/usuario/Desktop/Análise de Dados 2019")

# carregar Turmas_1

Turmas_1<-read.csv2.ffdf(file="TURMAS_1.csv",sep="|",first.rows=100000)

#Salvar em data.frame

Turmas_1 <- as.data.frame(Turmas_1)

# Filtrar os registros referentes ao Estado de Pernambuco (C0_UF == 26)

turmas_pe <- subset(Turmas_1, Turmas_1$CO_UF == "26") 

# Salvar em R.data

save(turmas_pe, file = "turmas_pe.RData")

## Questão 5 

setwd("C:/Users/usuario/Desktop/Análise de Dados 2019")

load("turmas_pe.RData")

# Média do número de matrícula por turmas 

mean(turmas_pe$NU_MATRICULAS)
# Resultado: 23.07089

## Questão 6 

# carregar DOCENTES_NORDESTE

docentes_ne <- read.csv2.ffdf(file = 'DOCENTES_NORDESTE.csv', sep = '|', first.rows = 100000)

# Filtrar os registros referentes ao Estado de Pernambuco (C0_UF == 26)

docentes_PE <- subset(docentes_ne, CO_UF == '26')

# transformar em tabela

tabela_docentes <- table(docentes_PE$TP_COR_RACA)

# Percentual de docentes de PE que não declararam raça (0)/ que se declararam pretos e pardos (2 e 3)

prop.table(tabela_docentes)
# Resultado: 44% não declararam cor/raça e 31,36409% se declararam preto ou pardo 
