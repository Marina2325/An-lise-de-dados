## Análise de dados - lista 11 
## Marina Laporte Cotias 

## Questão 1 

## a) 

## O Modelo Linear de Probabilidade (LPM) é um modelo de mínimos quadrados ordinários em que a 
## variável dependente é uma variável dummy. É chamado de modelo de probabilidade, pois podemos 
## interpretar os valores de Y como probabilidades previstas. 

## b) 

## Os valores estimados podem ser  interpretados como a probabilidade de que a variável dependente 
## dummy seja igual a 1.

## c) 

## Os coeficientes estimados em um modelo de probabilidade são interpretados de forma que um aumento 
## de 1 unidade na variável independente apresenta um efeito de Beta% na probabilidade da variável 
## dependente ser igual a 1. 

## d) 

## Um modelo linear baseia-se no pressuposto de que o resultado é contínuo e assume a suposição de 
## linearidade, com erros normalmente distribuídos. Mas quando se tem uma variável dependente binária, 
## essa suposição é fortemente violada. Além disso, problemas como a não-normalidade dos termos do erro, 
## heterocedasticidade e previsões fora o intervalo de 0 e 1, podem estar presente quando usado um modelo 
## linear para uma variável dependente binária. 

## Questão 2 

## a) 

## É um modelo de regressão em que a variável dependente é uma variável categórica, frequentemente binária. 
## Além disso, em um modelo logit os erros apresentam distribuição logística, média zero e a variância dos 
## erros igual a π²/ 3.  

## b) 

## Com a mudança de 1 unidade na variável independente, o log odds da variável dependente tem um efeito de B
## na variável dependente. 

## c) 

## Os coeficientes estimados indicam a direção dos efeitos parciais da variável independente, de acordo 
## com os sinais (se é positivo ou negativo) sobre a probabilidade de resposta. seus valores são 
## responsáveis por dar a mudança no log-odds do resultado para um aumento de uma unidade na variável 
## preditora.

## Questão 3 

## a) 

## É um modelo de regressão em que a variável dependente é binária, com a distribuição normal dos erros e variância 
## dos erros igual a 1. O modelo probit estima a probabilidade dos possíveis resultados binários. 

## b) 

## A mudança de 1 unidade no coeficiente de regressão (B) apresenta um efeito no z-score da variável dependente. 

## c) 

## Assim como para modelos bivariados, o aumento de 1 unidade na variável independente causa um aumento de B 
## no score-z. 

## Questão 4 

## a) 

## A principal diferenças entre esses modelos é que a distribuição logística tem caudas ligeiramente menos 
## densas em comparação com a distribuição normal.

## b) 

## São variáveis que não são diretamente observadas no mundo real. Essas variáveis podem ser inferidas de 
## outras variáveis que são observadas. Além disso, elas aparecem no modelo em forma de resposta binária.

## c) 

## Os coeficientes de regressão logística correspondem a efeitos marginais, em que a unidade de medida é 
## em log-odds. Sendo esse modelo uma função não-linear, o efeito marginal expresso como proporção não é 
## estável em toda a faixa de dados. Com isso, o impacto marginal na mudança de 1 unidade na variável 
## independente apresenta um efeito em probabilidade na variável dependente. 


## Questão 5 

# Pacotes necessários 

install.packages("aod")
library(aod)
library(ggplot2)

# Banco de dados 

mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

# Analisando as primeiras linhas 

head(mydata) 

# Análise descritiva 

summary(mydata)

# Obtendo desvio padrão 

sapply(mydata, sd) 

# Garantir que não haja células com 0

xtabs(~admit + rank, data = mydata)

# Transformando "rank" em fator

mydata$rank <- factor(mydata$rank)

# Modelo de regressão logística 

mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")

summary(mylogit)

# Obtendo intervlos de confiança dos coeficientes estimados 
            ## Usando a probabilidade em Log 

confint(mylogit) 

# Usando o erro padrão

confint.default(mylogit)

# Testar efeito geral para Rank

wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)

# Testar hipóteses adicionais sobre as diferenças nos coeficientes para os diferentes 
# níveis de classificação:  

# Identificar se o coeficiente para rank = 2 é igual ao coeficiente para rank = 3.

l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = l)

# Exponenciando os coeficientes para interpretá-los como odds-ratios 

exp(coef(mylogit)) 

# Odds ratio e  IC 95% 

exp(cbind(OR = coef(mylogit), confint(mylogit))) 

## Calcular a probabilidade prevista de admissão em cada valor de classificação, mantendo gre 
## e gpa em suas médias:  

# Vizualisar quadro de dados 

newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4))) 

# Analisando data.frame ]

newdata1 

# Criando probabilidades 

newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")

newdata1

# Criando 100 valores de gre entre 200 e 800, em cada valor de rank 

newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),
                                              4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))

newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# Analisar os dados 

head(newdata3) 

# Gráfico das probabilidades previstas e intervalos de confiança de 95%

ggplot(newdata3, aes(x = gre, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rank), size = 1)

## Analisar a adequação do modelo: 

# Encontrar a diferença no desvio para os dois modelos

with(mylogit, null.deviance - deviance) 

# Os graus de liberdade para a diferença entre os dois modelos é igual ao número de 
# variáveis preditoras no modo:

with(mylogit, df.null - df.residual)

with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

# probabilidade do log do modelo

logLik(mylogit)

## Questão 6: 

# Instalar pacotes:

install.packages("aod")
require(aod)
require(ggplot2)

# Carregar base de dados:

dados <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

# Converter variável categórica "rank" para um fator:

dados$rank <- factor(dados$rank)

# Analisar primeiras linhas da base:

head(dados)

# Analisar estatísticas descritivas da base:

summary(dados)

# Analisar variável rank vs admit (a variável dependente): 

xtabs(~rank + admit, data = dados)

# Modelo probit (dado que a variável de resposta é binária), VD = admit,
# VIs = gre, gpa e rank:

regprobit <- glm(admit ~ gre + gpa + rank, family = binomial(link = "probit"), 
                 data = dados)


summary(regprobit)

# intervalo de confiança para os coeficientes do modelo:

confint(regprobit)

# Efeito da VI "rank":

wald.test(b = coef(regprobit), Sigma = vcov(regprobit), Terms = 4:6)

### O efeito de rank é estatisticamente significante.

# Testar a diferença entre os termos 4 e 5 do modelo de regressão (rank = 2 e 
# rank = 3):

l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(regprobit), Sigma = vcov(regprobit), L = l)

### A diferença entre os coeficientes de rank = 2 e rank = 3 é 
### estatisticamente significante.

# Criar nova base com probabilidades esperadas das VIs (criar um "null model"): 

novosdados <- data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100), 
                                   4 * 4), gpa = rep(c(2.5, 3, 3.5, 4), each = 100 * 4), 
                         rank = factor(rep(rep(1:4, each = 100), 4)))

# Analisar primeiras linhas da nova base:

head(novosdados)

## Criar quatro plots com as probabilidades esperadas para os diferentes escores
## da VI "gre", um para cada nível da VI "gpa" e cada cor de linha representa
## uma categoria de "rank" e IC 95%: 

# Acrescentar valores do erro padrão dos coeficientes e probabilidade predita
# à base:

novosdados[, c("p", "se")] <- predict(regprobit, novosdados, type = "response", 
                                      se.fit = TRUE)[-3]

# Gráficos:

ggplot(novosdados, aes(x = gre, y = p, colour = rank)) + geom_line() + facet_wrap(~gpa)

# Diferença entre a variância dos resíduos do modelo nulo e o construído: 

with(regprobit, null.deviance - deviance)

# Diferença nos graus de liberdade entre o modelo nulo e o construído:

with(regprobit, df.null - df.residual)

# Obter p-valor a partir do teste do qui-quadrado:

with(regprobit, pchisq(null.deviance - deviance, df.null - df.residual, 
                      lower.tail = FALSE))


## Questão 7

# Adicionar valores dos intervalos de confiança à base:

novosdados <- within(novosdados, {
  PredictedProb <- novosdados$p
  lwr <- PredictedProb - (1.96 * se)
  upr <- PredictedProb + (1.96 * se)
})

# Analisar base:

head(novosdados)

# Gráficos com intervalos de confiança:

ggplot(novosdados, aes(x = gre, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = rank), alpha = 0.2) +
  geom_line(aes(colour = rank), size = 0.7) + facet_wrap(~gpa) 

## Questão 8

# Pacotes necessários

if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)

# Base de dados 

sample <- read.csv("C:/Users/marin/OneDrive/Área de Trabalho/Lista 10/sample.csv", header=T)

## a)

# Modelo logit 

reg <- glm(hon ~ female, family = binomial, data = sample)

summary(reg)

# Transformar os coeficientes do modelo em probabilidade

exp( coef(reg) )

## Sendo "female" igual a 1, os resultados mostram um aumento na log odds de hon em 0.59. 
## Tranformando em probabilidade, podemos observar que as mulheres apresentam 1.8 vezes mais 
## chances do que os homens de passarem para a "honor class". 

## b)

# Modelo logit 2

reg2 <- glm(hon ~ math, family = binomial, data = sample)

summary(reg2)

# Transformar os coeficientes do modelo em probabilidade

exp( coef(reg2) )

## O aumento de 1 unidade de "math" apresenta um aumento na log-odds de hon em 0.15634.
## Transformando os coeficientes estimados em probabilidades, podemos observar que um 
## indivíduo com valor correspondente a mediana de math, apresenta 1.16 de chances de 
## hon ser igual a 1. 

## c)

# Modelo logit 3

reg3 <- glm(hon ~ female + math + read, family = binomial, data = sample)

summary(reg3)

# Probabilidade predita para da admissão de cada valor de female e math, deixando read na sua média.

sample.2 <- with(sample, data.frame(female, math, read = mean(read)))

# Transformação dos valores previstos e os limites de confiança em probabilidades.

sample.2 <- cbind(sample.2, predict(reg3, newdata = sample.2, type = "link", se = TRUE))
sample.2 <- within(sample.2, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# Colocar female como fator

sample.2$female <- factor(sample.2$female)

# Gráfico do comportamento do modelo para “female” em relação a “math”, com média de read.

ggplot(sample.2, aes(x = math, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = female), alpha = 0.2) + 
  geom_line(aes(colour = female))

## d)

# Pacote necessário  

if(require(rms) == F) install.packages('rms'); require(rms)

# Pseudo-R2 de Nagelkerke

reg3n <- lrm(hon ~ female + math + read, data = sample)

# Modelo de regressão incluindo o R2 de Nagelkerke

print(reg3n)

### R2 = 0.421 

## Questão 9:
  
# Instalar pacotes:

require(foreign)
install.packages("nnet")
require(nnet)
require(ggplot2)
install.packages("reshape2")
require(reshape2)

# Carregar banco de dados:

ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")

# Analisar variáveis de interesse:

with(ml, table(ses, prog))

# Estatísticas descritivas das variáveis:

with(ml, do.call(rbind, 
tapply(write, prog, function(x) c(M = mean(x), SD = sd(x)))))

# Definir categoria de referência ("academic"):

ml$prog2 <- relevel(ml$prog, ref = "academic")

# Regressão logística multinomial:

test <- multinom(prog2 ~ ses + write, data = ml)

summary(test)

# Calcular p-valor:

z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Extrair coeficientes do modelo e exponenciar valores, que aponta o "risk ratio":

exp(coef(test))

# Calcular probabilidade predita para cada resultado e analisar as primeiras linhas:

head(pp <- fitted(test))

## Analisar mudanças nas probabilidades preditas. A VI "write" será mantida constante 
## em sua média, enquanto serão analisadas as probabilidades preditas para cada um dos 
## níveis da VI "ses":

dses <- data.frame(ses = c("low", "middle", "high"), write = mean(ml$write))
predict(test, newdata = dses, "probs")

## Analisar as probabilidades preditas médias para os diferentes valores de "write",
## em cada um dos níveis da variável "ses":

dwrite <- data.frame(ses = rep(c("low", "middle", "high"), each = 41), 
                     write = rep(c(30:70), 3))

# Criar objeto com probabilidades preditas para cada um dos valores de "ses" e "write":

pp.write <- cbind(dwrite, predict(test, newdata = dwrite, type = "probs", se = TRUE))

# Calcular as probabilidades preditas médias para cada nível de "ses":

by(pp.write[, 3:5], pp.write$ses, colMeans)

# Utilizar função "melt" para transformar o objeto "pp.write" em formato "long":

lpp <- melt(pp.write, id.vars = c("ses", "write"), value.name = "probability")

# Analisar primeiras linhas:

head(lpp) 

# Gráfico das probabilidades preditas para os valores de "write" a cada nível de "ses",
# em cada um dos 3 tipos de programa ("academic", "general" ou "vocation") 

ggplot(lpp, aes(x = write, y = probability, colour = ses)) + geom_line() + 
  facet_grid(variable ~ ., scales = "free") 

## Questão 10 

# Pacotes necessários 

library(effects)
library(tidyr)

ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")

# Colocar "academic" como variável de referência

ml$prog2 <- relevel(ml$prog, ref = "academic") 

## Modelo multinomial 

test <- multinom(prog2 ~ ses + write, data = ml)

summary(test)

## Intervalo de confiança 

confint(test) 

## Agregando os dados 

fit.eff <- effect("ses", test)

testegrafico <- data.frame(fit.eff$model.matrix, fit.eff$prob, fit.eff$lower.prob, 
                           fit.eff$upper.prob)

testegrafico$seslow <- ifelse(testegrafico$sesmiddle == 0 & testegrafico$seshigh == 0,  1, 0)

testegrafico$ses <- ifelse(testegrafico$sesmiddle == 1, "middle", ifelse(testegrafico$seshigh == 1,  "high", "low"))

grupo1 <- gather(testegrafico, response, prob, prob.academic:prob.vocation)
grupo2 <- gather(testegrafico, LB, probLB, L.prob.academic:L.prob.vocation)
grupo3 <- gather(testegrafico, UB, probUB, U.prob.academic:U.prob.vocation)

# selecionando colunas de cada base

grafico <- cbind(grupo1[, 12:14], grupo2[, 13:14], grupo3[, 13:14])

# ajustando levels na nova base

grafico$ses <- factor(grafico$ses, levels = c("low", "middle", "high"))

# Gráfico com os intervalos de confiança

ggplot(grafico, aes(x = response, y = prob)) +
  geom_errorbar(aes(ymin = probLB, ymax = probUB), width = 0.2, lty=1, lwd=1, col="red") +
  geom_point(shape=18, size=5, fill="black") +
  facet_wrap(~ses)


## Questão 11:

## a) 
  
# Carregar banco de dados:

ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")

# Analisar variáveis de interesse:

with(ml, table(ses, prog))

# Estatísticas descritivas das variáveis:

with(ml, do.call(rbind, tapply(write, prog, function(x) c(M = mean(x),
                                                          SD = sd(x)))))

# Definir categoria de referência ("vocation"):

ml$prog3 <- relevel(ml$prog, ref = "vocation")

# Regressão logística multinomial:

test2 <- multinom(prog3 ~ ses + write, data = ml)

summary(test2)

# Calcular p-valor:

z2 <- summary(test2)$coefficients/summary(test2)$standard.errors
p2 <- (1 - pnorm(abs(z2), 0, 1)) * 2
p2

# Extrair coeficientes do modelo e exponenciar valores, que aponta o "risk ratio"
# relativo para o aumento de uma unidade na VI:

exp(coef(test2))

# Calcular a probabilidade predita para cada resultado e analisar as primeiras linhas:

head(pp <- fitted(test2))

# Analisar mudanças nas probabilidades preditas. Neste caso, a VI "write" será
# mantida constante em sua média, enquanto serão analisadas as probabilidades
# preditas para cada um dos níveis da VI "ses":

dses2 <- data.frame(ses = c("low", "middle", "high"), write = mean(ml$write))
predict(test2, newdata = dses2, "probs")

# Probabilidades preditas médias para os diferentes valores de "write",
# em cada um dos níveis da variável "ses":

dwrite2 <- data.frame(ses = rep(c("low", "middle", "high"), each = 41), 
                     write = rep(c(30:70), 3))

# Criar objeto com probabilidades preditas para cada um dos valores de "ses" e "write":

pp.write2 <- cbind(dwrite2, predict(test2, newdata = dwrite2, type = "probs", se = TRUE))

# Probabilidades preditas médias para cada nível de "ses":

by(pp.write2[, 3:5], pp.write2$ses, colMeans)

# Utilizar função "melt"  para transformar o objeto "pp.write" em formato "long":

lpp2 <- melt(pp.write2, id.vars = c("ses", "write"), value.name = "probability")

# Analisar primeiras linhas:

head(lpp2) 

# Gráfico das  probabilidades preditas para os valores de "write" a cada nível de "ses",
# em cada um dos 3 tipos de programa ("academic", "general" ou "vocation") 

ggplot(lpp2, aes(x = write, y = probability, colour = ses)) + geom_line() + 
  facet_grid(variable ~ ., scales = "free") 


## Analisando os resultados, podemos observar que alunos com um status socioeconômico alto, 
## comparados aos de nível socioeconômico baixo, apresentam  maior probabilidade de escolher 
## o tipo de programa “acadêmico” durante o ensino médio, em comparação ao programa do tipo 
## "vocation" e menor probabilidade de escolher programas do tipo "general" do que aqueles do 
## tipo "vocation". Os alunos com nível socioeconômico médio, comparados aos de nível socioeconômico 
## baixo, têm menor probabilidade de escolher programas dos tipos "general" ou  "academic" quando 
## comparados à categoria base "vocation". O p-valor dos coeficientes da análise foram não significativos, 
## tendo como referência um nível de significância de 0.05.
  
## Mantendo constante a variável “write” constante (em sua média), podemos observar as probabilidades 
## preditas para cada nível da VI "ses" e tipo de programa (a VD). Os alunos que escolhem o programa 
## “vocation” apresentam uma maior probabilidade de pertencer à classe socioeconômica média. Para os 
## alunos que optam pelo programa “general”, a probabilidade que esse pertença a uma classe socioeconômica 
## baixa é maior. Por fim, para os que optam por programas do tipo "academic", a probabilidade de que pertençam 
## à classe socioeconômica alta é maior.

## b)

# Carregar dados:

ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")

# Analisar variáveis de interesse:

with(ml, table(ses, prog))

# Obter estatísticas descritivas das variáveis:

with(ml, do.call(rbind, tapply(write, prog, function(x) c(M = mean(x),
                                                          SD = sd(x)))))
# Definir categoria de referência ("general"):

ml$prog4 <- relevel(ml$prog, ref = "general")

# Regressão logística multinomial:

test3 <- multinom(prog4 ~ ses + write, data = ml)

# Checar resultados da regressão:

summary(test3)

# Calcular p-valor:

z3 <- summary(test3)$coefficients/summary(test3)$standard.errors
p3 <- (1 - pnorm(abs(z3), 0, 1)) * 2
p3

# Extrair coeficientes do modelo e exponenciar valores, que aponta o "risk ratio"
# relativo para o aumento de uma unidade na VI:

exp(coef(test3))

# Probabilidade predita para cada resultado e checar primeiras linhas:

head(pp <- fitted(test3))

# Analisar mudanças nas probabilidades preditas. A VI "write" será mantida constante, 
## em sua média, enquanto serão analisadas as probabilidades preditas para cada um dos 
## níveis da VI "ses":

dses3 <- data.frame(ses = c("low", "middle", "high"), write = mean(ml$write))
predict(test3, newdata = dses3, "probs")

# Analisar as probabilidades preditas médias para os diferentes valores de "write",
# em cada nível da variável "ses":

dwrite3 <- data.frame(ses = rep(c("low", "middle", "high"), each = 41), 
                      write = rep(c(30:70), 3))

# Probabilidades preditas para cada um dos valores de "ses" e "write":

pp.write3 <- cbind(dwrite3, predict(test3, newdata = dwrite3, type = "probs", se = TRUE))

# Calcular as probabilidades preditas médias para cada nível de "ses":

by(pp.write3[, 3:5], pp.write3$ses, colMeans)

# Utilizar função "melt" para transformar o objeto "pp.write" em formato "long":

lpp3 <- melt(pp.write3, id.vars = c("ses", "write"), value.name = "probability")

# Analisar primeiras linhas:

head(lpp3) 

# Gráfico das probabilidades preditas para os valores de "write" a cada nível de "ses",
# em cada um dos 3 tipos de programa ("academic", "general" ou "vocation") 

ggplot(lpp3, aes(x = write, y = probability, colour = ses)) + geom_line() + 
  facet_grid(variable ~ ., scales = "free")

## Analisando os resultados, podemos observar que os alunos com um status socioeconômico alto, 
## comparando com os nível socioeconômico baixo , apresentam maior probabilidade de escolher o 
## tipo de programa “acadêmico” durante o ensino médio. Comparando com o programa “general”, os 
## alunos de classe alta também apresentam uma maior probabilidade de escolher programas do tipo 
## “vocation” do que do tipo “general”.  Já os alunos com socioeconômico médio, comparados aos de 
## nível socioeconômico baixo, têm maior probabilidade de escolher programas dos tipos "vocation" 
## ou "academic" quando comparados à categoria base "general”. O p-valor dos coeficientes da análise 
## foram não significativos, tendo como referência um nível de significância de 0.05 (com exceção dos
## interceptos). 

## Questão 12 

## a) 

# Pacotes necessários 

install.packages('arm')
install.packages('effects')
library(arm)
library(effects)
library(nnet)
require(foreign)
require(reshape2)

## Definir diretório e carregar base de dados 

setwd('C:/Users/marin/OneDrive/Área de Trabalho/Lista 10')

df <- read.dta('nomocc2.dta')

##  Transformar as variáveis Menial, BlueCol e Craft em uma única variável 'Manual"

df$wkr <- df$occ

df$wkr <- ifelse(df$wkr %in% c('Menial','BlueCol','Craft'),'Manual',as.character(df$wkr))

df$wkr <- as.factor(df$wkr)

## Alterar Categoria de Referência

df$wbk <- relevel(df$wkr, ref = "Manual")

## White para factor

df$white <- as.factor(df$white)


## Modelo multinomial 

md <- multinom(wbk ~ white + exper + ed,data = df) 

summary(md)


## O aumento de uma unidade na variável white está associado a um aumento nas
## log odds de estar na categoria whitecol versus manual no valor de 1.01. 

## O aumento de uma unidade na variável white está associado a um aumento nas
## log odds de estar na categoria prof versus manual no valor de 1.24. 

## b) 

## Gerando Previsões: exper fixo 

library(effects)

prv <- effect('white', md, xlevels = list(0,1), fixed.predictiors = 'exper',
              given.values = mean(df$exper), se = T)

to.plot <- data.frame(p.nw = prv$prob[1,],
                      p.wh = prv$prob[2,],
                      se.nw = prv$se.prob[1,],
                      se.wh = prv$se.prob[2,])

# Plot 

par(mfrow = c(1,2))
coefplot(to.plot$p.nw,to.plot$se.nw,CI = 2,main = 'Not white',vertical = F,
         varnames = c('Manual','Prof','WhiteCol'), ylim = c(0,.9),
         cex.var = 1.2, cex.pts = 2, ylab = 'Probabilidades',var.las = 1)

coefplot(to.plot$p.wh,to.plot$se.wh,CI = 2,main = 'White',vertical = F,
         varnames = c('Manual','Prof','WhiteCol'), ylim = c(0,.9),
         cex.var = 1.2, cex.pts = 2, ylab = 'Probabilidades',var.las = 1)


## Questão 13 

# Gerando Previsões: para wrk com exper fixo WHITE = 1

ppv <- effect('ed',md,xlevels = list(ed = 3:20),fixed.predictiors = c('white', 'exper'), 
              given.values = c('1', mean(df$exper), se = T))



## Criando data.frame para construir o gráfico 

to.plot <- data.frame(ma.prob = ppv$prob[,1],
                      pr.prob = ppv$prob[,2],
                      wh.prob = ppv$prob[,3],
                      ma.se = ppv$se.prob[,1],
                      pr.se = ppv$se.prob[,2],
                      wh.se = ppv$se.prob[,3])

par(mfrow = c(1,3))

## Plot

plot(to.plot$ma.prob ~ c(3:20), type = 'l',
     main = 'Manual', ylab = 'Probabilidade',
     xlab = 'ed', col = 'red',
     ylim = c(min(to.plot$ma.prob - 2*to.plot$ma.se),
              max(to.plot$ma.prob + 2*to.plot$ma.se)))

lines(y = to.plot$ma.prob - 2*to.plot$ma.se,x = c(3:20),
      col = 'blue')
lines(y = to.plot$ma.prob + 2*to.plot$ma.se,x = c(3:20),
      col = 'blue')

plot(to.plot$pr.prob ~ c(3:20), type = 'l',
     main = 'Profissional', ylab = 'Probabilidade',
     xlab = 'ed', col = 'red',
     ylim = c(min(to.plot$pr.prob - 2*to.plot$pr.se),
              max(to.plot$pr.prob + 2*to.plot$pr.se)))

lines(y = to.plot$pr.prob - 2*to.plot$pr.se,x = c(3:20),
      col = 'blue')
lines(y = to.plot$pr.prob + 2*to.plot$pr.se,x = c(3:20),
      col = 'blue')

plot(to.plot$wh.prob ~ c(3:20), type = 'l',
     main = 'White Collar', ylab = 'Probabilidade',
     xlab = 'ed', col = 'red',
     ylim = c(min(to.plot$wh.prob - 2*to.plot$wh.se),
              max(to.plot$wh.prob + 2*to.plot$wh.se)))

lines(y = to.plot$wh.prob - 2*to.plot$wh.se,x = c(3:20),
      col = 'blue')
lines(y = to.plot$wh.prob + 2*to.plot$wh.se,x = c(3:20),
      col = 'blue')


# Gerando Previsões para wrk com exper fixo WHITE = 0

ppv <- effect('ed',md,xlevels = list(ed = 3:20),fixed.predictiors = c('white', 'exper'), 
              given.values = c('0', mean(df$exper), se = T))


# Criando data.frame para gerar gráfico 

to.plot <- data.frame(ma.prob = ppv$prob[,1],
                      pr.prob = ppv$prob[,2],
                      wh.prob = ppv$prob[,3],
                      ma.se = ppv$se.prob[,1],
                      pr.se = ppv$se.prob[,2],
                      wh.se = ppv$se.prob[,3])

par(mfrow = c(1,3))

# Plot 

plot(to.plot$ma.prob ~ c(3:20), type = 'l',
     main = 'Manual', ylab = 'Probabilidade',
     xlab = 'ed', col = 'red',
     ylim = c(min(to.plot$ma.prob - 2*to.plot$ma.se),
              max(to.plot$ma.prob + 2*to.plot$ma.se)))

lines(y = to.plot$ma.prob - 2*to.plot$ma.se,x = c(3:20),
      col = 'blue')
lines(y = to.plot$ma.prob + 2*to.plot$ma.se,x = c(3:20),
      col = 'blue')

plot(to.plot$pr.prob ~ c(3:20), type = 'l',
     main = 'Profissional', ylab = 'Probabilidade',
     xlab = 'ed', col = 'red',
     ylim = c(min(to.plot$pr.prob - 2*to.plot$pr.se),
              max(to.plot$pr.prob + 2*to.plot$pr.se)))

lines(y = to.plot$pr.prob - 2*to.plot$pr.se,x = c(3:20),
      col = 'blue')
lines(y = to.plot$pr.prob + 2*to.plot$pr.se,x = c(3:20),
      col = 'blue')

plot(to.plot$wh.prob ~ c(3:20), type = 'l',
     main = 'White Collar', ylab = 'Probabilidade',
     xlab = 'ed', col = 'red',
     ylim = c(min(to.plot$wh.prob - 2*to.plot$wh.se),
              max(to.plot$wh.prob + 2*to.plot$wh.se)))

lines(y = to.plot$wh.prob - 2*to.plot$wh.se,x = c(3:20),
      col = 'blue')
lines(y = to.plot$wh.prob + 2*to.plot$wh.se,x = c(3:20),
      col = 'blue')

