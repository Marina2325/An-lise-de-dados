# Lista 3 - Análie de dados (Marina laporte Cotias)

## Questão 2 

x <- 2 

y <- 4 

Z <- x + y

Z*11222936402
#Resposta: 67337618412

## Questão 3 

install.packages("ffbase", dependencies = TRUE)
install.packages("ff")

library("ffbase")
library("ff")

str(mtcars)
dim(mtcars)
mtcars[,3]
mtcars[2,]
mtcars$cyl[4]
summary(mtcars)



## Questão 4 

# modificar o diretório (caminho da pasta onde tão os arquivos)

setwd("C:/Users/usuario/Desktop/Análise de Dados 2019")

# para carregar o banco com ffbase

Turmas_1<-read.csv2.ffdf(file="TURMAS_1.csv",sep="|",first.rows=100000)

# filtrar dados de pernambuco (C0_UF == 26) 

Turmas_1 <- as.data.frame(Turmas_1)

turmas_pe <- subset(Turmas_1, Turmas_1$CO_UF == "26") 

# Salvar em R.data

save(turmas_pe, file = "turmas_pe.RData")

## Questão 5 

setwd("C:/Users/usuario/Desktop/Análise de Dados 2019")

load("turmas_pe.RData")

mean(turmas_pe$NU_MATRICULAS)
# Resultado: 23.07089

## Questão 6 

# carregar DOCENTES_NORDESTE
docentes_ne <- read.csv2.ffdf(file = 'DOCENTES_NORDESTE.csv', sep = '|', first.rows = 100000)

docentes_PE <- subset(docentes_ne, CO_UF == '26')
save.ffdf(docentes_PE, dir="C:/Users/usuario/Desktop/Análise de Dados 2019/docentes_PE")

# transforma em tabela
tabela_docentes <- table(docentes_PE$TP_COR_RACA)

#porcentagem
prop.table(tabela_docentes)
# 44% não declararam cor/raça e 31,36409% se declararam preto ou pardo 
