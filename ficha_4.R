install.packages('tidyverse')
library(magrittr)
require(tidyverse)
library(tidyverse)
library(ff)
library(ffbase)
# Questão 2

setwd("C:/Users/Lucas Albuquerque/Downloads/dados_encontro_2_ufpe/dados_encontro_2_ufpe")
load('docentes_pe_censo_escolar_2016.RData')
load('matricula_pe_censo_escolar_2016.RData')
load('turmas_pe_censo_escolar_2016.RData')
load('escolas_pe_censo_escolar_2016.RData')
library(readxl)
atlas2013_dadosbrutos_pt <- read_excel("atlas2013_dadosbrutos_pt.xlsx")
## View(atlas2013_dadosbrutos_pt)



docentes_pe_selecao <- docentes_pe %>% filter(NU_IDADE <= 70, NU_IDADE >= 18)
matriculas_pe_selecao <- matricula_pe %>% filter(NU_IDADE >= 1, NU_IDADE <= 25)
IDHm_pe <- atlas2013_dadosbrutos_pt %>% filter(IDHM, UF == '26', ANO == '2010')
matriculas_pe_selecao$NU_MAT <- as.integer('1')
docentes_pe_selecao$NU_DOC <- as.integer('1')

agregado_matr <- aggregate(NU_MAT~CO_MUNICIPIO, data = matriculas_pe_selecao, sum)
agregado_doc <- aggregate(NU_DOC~CO_MUNICIPIO, data = docentes_pe_selecao, sum)
matricula_docentes <- merge(agregado_doc, agregado_matr)
matricula_docentes$mat_por_docente <- (agregado_matr$NU_MAT/agregado_doc$NU_DOC)
names(IDHm_pe)[4]<- 'CO_MUNICIPIO'
#estatísticas descritivas
summary(matricula_docentes$mat_por_docente)

#unir bancos
merge(matricula_docentes, IDHm_pe, by = 'CO_MUNICIPIO')

## municipio com maior número de matrículas por docente
# TUPANATINGA - 9.6 matrículas por docente
# IDHM = 0.519 

cor(matricula_docentes$mat_por_docente, IDHm_pe$IDHM, method = 'pearson')

# Questão 3 
# Gráfico Número de alunos por docente x IDHM
ggplot(matricula_docentes$mat_por_docente, IDHm_pe$IDHM, aes(x = matricula_docentes$mat_por_docente,y = matricula_docentes$IDHM))
