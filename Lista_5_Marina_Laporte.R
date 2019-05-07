#Análise de dados - Lista 5 
#Marina laporte Cotias

## Lista 1 

## Questão 5 

Comércio <- c(8,9,6,8,8,5,6) 

Industria <- c(4,5,3,6,6,5,4,4)

# a) classificar cada uma das variáveis 

str(Comércio)
# numérica 

str(Industria)
# numérica 

# b) média e mediana dos grupos 

mean(Comércio)
# 7.142857 

mean(Industria)
# 4.625 

median(Comércio)
# 8 

median(Industria)
# 4.5

# c) Desvio padrão dos dois grupos 

sd(Comércio)
# 1.46385 

sd(Industria)
# 1.06066 

# d) número máximo de meses / qual medida discritiva fornece essa informação 

crescimento_empresa <- c(8,9,4,5,3,6,8,6,6,8,5,5,6,4,4)

quantile(crescimento_empresa)

# e) média, mediana e desvio padrão dos tamanhos das empresas 

pequena <- c(6,6,5,5)

média <- c(9,5,3,6,8,6,4)

grande <- c(8,4,8,4)

mean(pequena)

median(pequena)

sd(pequena)

mean(média)

median(média)

sd(média)

mean(grande)

median(grande)

sd(grande)

## Questão 6 

# a) Investimento médio 

investimento <- c(26,16,14,10,19,15,19,16,19,18)

média_investimento <- mean(investimento)

# b) programa especial 

valor_corte <- média_investimento - 2*sd(investimento)
# 8.83

# nenhuma cidade receberá o subsídio

# c) Calcular o investimento básico 
valor_corte_2 <- média_investimento+2*sd(investimento)
install.packages('tidyverse')
require(tidyverse)

# investimento básico
invest_basico <-mean(subset(investimento, investimento < valor_corte_2, investimento > valor_corte))

## Questão 7 

estimulo_A <- c(55,2,13,11,23,2,15,12,14,28,12,45,19,30,16,12,7,13,1,7)


estimulo_b <- c(20,7,6,5,3,25,5,3,3,10,8,5,1,35,9,8,12,2,26) 
# a) média, mediana, variância e desvio padrão dos tempos de reação dos estímulos A e B

# Estímulo A 

mean(estimulo_A)

median(estimulo_A)

var(estimulo_A)

sd(estimulo_A)

# Estímulo B

mean(estimulo_b)

median(estimulo_b)

var(estimulo_b)

sd(estimulo_b)

# b) Boxplot 

boxplot(estimulo_A)

boxplot(estimulo_b)

## Questão 8 

# a) média, desvio padrão, a covariância e coeficiente de correlação

renda_sm <- c(12,16,18,20,28,30,40,48,50,54)
saude_perc <- c(7.2,7.4,7,6.5,6.6,6.7,6,5.6,6,5.5)
df <- data.frame(renda_sm,saude_perc)
mean(renda_sm)

sd(renda_sm)

mean(saude_perc)

sd(saude_perc)

cov(renda_sm,saude_perc)

cor(renda_sm,saude_perc)

# b) Gráfico de dispersão 

ggplot(df, aes(x = renda_sm, y = saude_perc)) + geom_point(color = 'pink', size = 3)+labs(x = 'número de salário mínimos', y = 'gasto com saúde')

## Questão 9 

Prova_1 <- c(7.5,8.2,8.5,8.7,8.8,9.1,9.2,9.3,10)

Prova_2 <- c(8.2,8,8.3,8.5,9.4,9.6,9,9.3,9.7)

Provas <- data.frame(Prova_1,Prova_2)

# correlação 

cor(Prova_1,Prova_2) 

# Gráfico de dispersão 

ggplot(Provas, aes(x = Prova_1, y = Prova_2)) + geom_point(color = 'pink', size = 3)+labs(x = 'Prova_1', y = 'Prova_2')

## Lista 2 

# Questão 5 

# distribuição binomial 

p <- 0.62

n <- 1000 

x <- 620


dbinom(x, n, p)

# a) média amostral 

# b) desvio padrão amostral 

# c) média populacional com nível de confiança de 95% 

## Questão 6 

install.packages("samplingbook")
require(samplingbook)

# a) Número de eleitores em que a proporção p seja estimada com um erro de 0,05, uma probailidade de o.95

sample.size.prop(0.05, P = 0.5, N = Inf, level = 0.95)

# b) Erro de 0.02 

sample.size.prop(0.02, P = 0.5, N= Inf, level = 0.95)

# c) candidato com 25% dos votos - tamanho amostral 

sample.size.prop(0.02, P = 0.25, N = Inf, level = 0.95)

# d) Intervalo de confiança de 95%

install.packages("Hmisc")
require(Hmisc)
install.packages("tibble")
require(tibble)

binconf(564, 2401, alpha = 0.05)

## Questão 11 

# a) Formular o problema em questão no formato de um teste de hipóteses com a H0 e Ha

# H0: não há associação entre a ideologia da legenda partidária e o posicionamento do parlamentar quanto 
# a descriminalização das drogas 
# H1: há associação entre a ideologia da legenda partidária e o posicionamento do parlamentar quanto 
# a descriminalização das drogas

# b) Erro tipo 1 e Erro tipo 2 

# 

# c) teste do Qui Quadrado 

favorável <- c(450,150)

Contrário <- c(100,300) 

esquerda <- c(450,100)

centro_direita <- c(150,300)

tabela <- data.frame(esquerda,centro_direita,row.names =  c("favorável", "contrário"))

chisq.test(tabela, correct = FALSE)

## Questão 12 

# Câmara 

house_1 <- c(87,88,97,85,94)

house_2 <- c(88,96,94,91,90,95,98,98,96,88,90,94,98,98,96,98,94)

t.test(house_2, house_1)


# Senado 

senate_1 <- c(85,88,71,77,74)

senate_2 <- c(85,64,60,55,93,90,75,85,96,83,92,91,90,79,86,96,79)

t.test(senate_1, senate_2)

## Questão 13 

# a) Problema no formato de teste de hipóteses 

# H0: Não há correlação entre o percentual de variação do PIB e o percentual de votos 
# recebidos pelo partido incumbente 
# H1: Há correlação entre o percentual de variação do PIB e o percentual de votos 
# recebidos pelo candidato do partido incumbente 

# b) coeficiente de correlação 

growth <- c(5.11,3.879,1.589,-5.553,2.763,-10.024,-1.425,-2.421,-6.281,4.164,2.229,-11.463,-3.872,4.623,-14.586)

vote <- c(48.516,50.22,49.846,50.414,48.268,47.76,53.171,60.006,54.483,54.708,51.682,36.148,58.263,58.756,40.851)

cor.test(growth, vote)

# c) Comparar o resultado obtido ao final da crise de 1929 com os resultados até 2008

# Comparando os resultados obtidos, é possível verificar que o aumento no número de resgitsros 
# na série histórica permitiu rejeitar a hiótese nula com p-valor. 