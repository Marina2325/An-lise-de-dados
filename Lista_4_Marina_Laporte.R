# Lista 4 - Análise de Dados 
# Marina Laporte Cotias

install.packages('tidyverse')
install.packages("readxl")


## Questão 1 

# https://github.com/Marina2325/Analise-de-Dados-2019

## Questão 2

# Definir diretório

setwd("C:/Users/usuario/Desktop/dados lista 4/dados_encontro_2_ufpe/dados_encontro_2_ufpe")

# Carregar base de dados 

load('docentes_pe_censo_escolar_2016.RData')
load('matricula_pe_censo_escolar_2016.RData')
load('turmas_pe_censo_escolar_2016.RData')
load('escolas_pe_censo_escolar_2016.RData')

# Carregar dados em XLS

require(readxl)

PNUD <- read_excel("atlas2013_dadosbrutos_pt.xlsx")

# Criar data frames com o filtro de idade
# e colunas específicas 

require(dplyr)

docentes_pe_selecao <- docentes_pe %>% filter(NU_IDADE <= 70, NU_IDADE >= 18) %>% select(CO_MUNICIPIO, CO_PESSOA_FISICA)%>% group_by(CO_MUNICIPIO)%>% summarise(DOC_MUN = n())

matriculas_pe_selecao <- matricula_pe %>% filter(NU_IDADE >= 1, NU_IDADE <= 25) %>% select(CO_MUNICIPIO, ID_MATRICULA)%>% group_by(CO_MUNICIPIO)%>% summarise(MAT_MUN = n()) 

IDHM_pe <- PNUD %>% filter(UF == '26', ANO == '2010')%>% select(ANO,Município,IDHM, Codmun7)

# Alterar nome da quarta coluna de IDHM_PE

names(IDHM_pe)[4]<- 'CO_MUNICIPIO'

# Unir tabelas

require(plyr)

tabela_IDHM_mat_docentes <- join_all(list(docentes_pe_selecao, matriculas_pe_selecao, IDHM_pe), by = 'CO_MUNICIPIO')

tabela_IDHM_mat_docentes$MAT_por_DOC <- tabela_IDHM_mat_docentes$MAT_MUN/tabela_IDHM_mat_docentes$DOC_MUN

# Colocar em Ordem decrescente 

tabela_IDHM_mat_docentes <- arrange(tabela_IDHM_mat_docentes, desc(MAT_por_DOC))

# Visualizar as primeiras observações de tabela_IDHM_mat_docentes

head(tabela_IDHM_mat_docentes)

# municipio com maior número de matrículas por docente
# TUPANATINGA - 9.6 matrículas por docente
# IDHM = 0.519 

# Estatísticas descritivas do número de alunos por docentes de PE

summary(tabela_IDHM_mat_docentes)

# MAT_por_DOC 
# Min.   :4.431     
# 1st Qu.:5.464     
# Median :5.945   
# Mean   :6.043  
# 3rd Qu.:6.584  
# Max.   :9.557 

# Correlação entre matricula por docente e IDHm

cor(tabela_IDHM_mat_docentes$MAT_por_DOC, tabela_IDHM_mat_docentes$IDHM, method = 'pearson')
# Resultado: - 0.5057435

# Salvar em R.Data 

save(tabela_IDHM_mat_docentes, file = "tabela_IDHM_mat_docentes.RData")

## Questão 3 

require(ggplot2)

# Gráfico Número de alunos por docente x IDHM
ggplot(tabela_IDHM_mat_docentes, aes(x = MAT_por_DOC, y = IDHM)) + geom_point(color = 'pink', size = 3)+labs(x = 'Número de matrículas por docente', y = 'IDHM')



