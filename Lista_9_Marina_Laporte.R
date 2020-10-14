## Análise de Dados - Lista 9 
## Marina Laporte Cotias 

## Questão 1

## a) Você está confiante de que a correlação entre Zi e Xi é zero 

## Sendo zi e xi totalmente independentes, ou seja, a correlação entre eles é zero, em que ambas variáveis 
## afetam Yi, podemos omitir com segurança a variável Zi sem que essa interfira no coeficiente estimado do 
## modelo. Porém, nesse caso as conclusões da variável dependente estarão distorcidas. 

## b) Você acha que a correlação entre Zi e Xi é positiva 

## Sendo Xi e Zi correlacionados, no qual ambas apresentam efeito na Yi, caso a variável Zi seja omitida 
## o termo de polarização não será igual a zero. Com isso, omitindo essa variável, o efeito de Xi em Yi estará
## sendo estimado de forma errada (poluindo esse efeito). É necessário controlar por Zi, para se obter uma estimativa 
## imparcial do efeito de Xi. Quando não incluída a variável Zi, temos um problema de viés de variável omitida, quando 
## o valor esperado da estimativa do parâmetro que obtemos da amostra não será igual ao parâmetro real verdadeiro. 
## Podemos análisar a magnitude desse viés. Se no modelo incluindo Zi, o coeficientes de Xi e Zi forem positivos, isso 
## significa que em um modelo em que a variável Zi for omitida, o coeficiente de Xi será maior do que deveria. Isso acontece 
## pois, um número positivo mais o produto de dois números positivos, terá como resultado um número positivo maior, ou seja, 
## um coeficiente superestimado. 

## Você acha que a correlação entre Zi e Xi é negativa

## Caso a correlação entre Zi e Yi seja negativa e Zi tenha um efeito negativo em Yi, o coeficiente estimado de Xi, sem incluir 
## o Zi no modelo, será maior do que deveria, pois um número negativo mais o produto de um número negativo com um positivo 
## terá como resultado um número negativo maior, ou seja, um coeficiente superestimado.

## Questão 2 

## Resultados da tabela 9.4 (analisando as colunas A e B)

## Na coluna A, os resultados mostram, que um aumento de 1 unidade na porcentagem de residentes do estado com um diploma 
## universitário, espera-se um aumento de 704.02 no salário dos professores. Esse efeito é significativo (p-valor<0.05) 
## e com um erro padrão de 140.22. Caso a variável da porcentagem dos residentes com diploma for zero, espera-se que 
## que o salário dos professores seja 2.8768,01, com um erro padrão de 3913.27. Esse efeito é significativo (p-valor<0.05). 
## Na coluna B, os resultados mostram, que com um aumento de 1 unidade na renda per capita, espera-se um aumento de 0.68 
## no salário dos professores. Esse efeito é significativo, com um erro padrão de 0.11. Caso a variável renda per capita 
## fosse zero, espera- se que o salário dos professores seja 2.1168,11. Esse efeito é significativo com um erro padrão 
## de 4102.40. 

## Questão 3 

## Analisando a coluna C é possível observar que com aumento de 1 unidade na porcentagem de residentes com diploma 
## universitário, espera-se um aumento de 24.58 no salário dos professores, controlando pela renda per capita. 
## Nesse modelo, a variável da porcentagem de residentes com diploma, passou a ser não-significativa e teve uma diminuição 
## substancial. Já a variável renda per capita, no contexto multivariado, permaneceu quase inalterada, controlando
## pela porcentagem de residentes com diploma, com um efeito de 0.66. Além disso, em relação a capacidade explicativa 
## do modelo, o R2 da coluna C em relação a coluna A, tem uma maior capacidade explicativa do modelo. Já em relação ao 
## modelo da coluna B, o R2 permaneceu inalterado. 

## Questão 4 

## 4.1 

## Definir diretório 

setwd('C:/Users/usuario/Desktop/lista 9 dados')

## Ler arquivo em txt 

bd <- read.delim('wordrecall.txt')

## Modelo 1 

reg <- lm(prop ~ time, data = bd)

summary(reg)

## Pressusposto de linearidade 

## Gráfico de dispersão ajustado 

library(ggplot2)

ggplot(data = bd, aes(y = prop, x = time)) + geom_point(color = "blue") + 
theme_classic() + geom_smooth(method = "lm", color = "black", se = FALSE) + 
geom_label(aes(x = 7000, y = 0.6), hjust = 0, 
label = paste("Adj R2 = ",signif(summary(reg)$adj.r.squared, 2),
 " \nP =",signif(summary(reg)$coef[2,4], 2)))


## Gráfico dos resíduos versus valores ajustados 

ggplot(lm(reg)) + geom_point(aes(x = .fitted, y = .resid)) + geom_abline(slope = 0)

## Gráfico de normalidade 

shap <- shapiro.test(reg$residuals)

ggplot(data = bd, aes(sample = reg$residuals)) + 
stat_qq(color = 'blue') + stat_qq_line(color = 'red', lty = 2) + 
geom_label(aes(x = 0, y = 0.4), hjust = 0, 
label = paste ("shapiro.test =",signif(summary(shap$p.value)[3], 5)))

## rmse modelo 1 

rmse <- function(x){
        sqrt(mean(x^2))  
}

rmse(reg$residuals)


## Modelo 2: level-log 

reg2 <- lm(prop ~ log(time), data = bd)

summary(reg2)

## Função de logarítimo natural 

bd$lntime <- log(bd$time) 

## Gráfico de dispersão com  x = ln.time 

ggplot(data = bd, aes(y = prop, x = bd$lntime)) + geom_point(color = "blue") + 
theme_classic() + geom_smooth(method = "lm", color = "black", se = FALSE) +
geom_label(aes(x = 6, y = 0.8), hjust = 0, 
label = paste("Adj R2 = ",signif(summary(reg2)$adj.r.squared, 2),
" \nP =",signif(summary(reg2)$coef[2,4], 2)))

## Gráfico dos resíduos versus valores ajustados com x = line.time 

ggplot(lm(reg2)) + geom_point(aes(x = .fitted, y = .resid)) + geom_abline(slope = 0)

## RMSE modelo 2

rmse(reg2$residuals)

## Gráfico de normalidade 

shap2 <- shapiro.test(reg2$residuals)

ggplot(data = bd, aes(sample = reg2$residuals)) + 
stat_qq(color = 'blue') + stat_qq_line(color = 'red', lty = 2) + 
geom_label(aes(x = 0, y = 0.1), hjust = 1, 
label = paste ("shapiro.test =",signif(summary(shap2$p.value)[3], 2)))

## a) Analisando o ajuste, o modelo 1 apresenta um erro padrão residual de 0.15, o que indica  que a média da 
## amostra se desvia em 0.15 das possíveis médias amostrais. RMSE foi de 0.14. Isso indica que o modelo desvia 
## em média 0.14 pontos ao prever a proporção de itens corretamente lembrados.O gráfico de dispersão do modelo 1, 
## sugere que a proporção de itens recuperados não está linearmente relacionada com o tempo. O gráfico dos resíduos 
## versus os valores ajustados, também sugere um relacionamento não-linear entre as variáveis. Como o modelo não é 
## linear, não é possível avaliar se as variâncias do erro são iguais. Analisando o gráfico da normalidade dos erros
## é possível ver que os pontos estão próximos a linha o que sugere evidências de que a distribuição dos erros é 
## normal. Além disso, o teste de Shapiro indica que não podemos rejeitar a hipótese nula de que a distribuição 
## dos erros é normal. Como o único problema do conjunto de dados é a não-linearidade, foi feita uma transformação 
## logarítimica no valor do preditor (linear-log). Com isso, os resultados mostram que 1% na variação do tempo, em minutos, desde 
## que a lista foi memorizada, implica em -0.00079 de variação na proporção de itens corretamente lembrados, com um p-valor significativo.  
## Em relação ao ajuste, o modelo 1 apresenta um erro padrão residual maior (0.15), do que o modelo 2 (0.023).Isso significa, 
## que no segundo modelo, a distância média entre os pontos do modelo e a reta de regressão é maior e com isso menos ajustado. 
## O RMSE do modelo 2 é maior do que o modelo 1 (0.21 > 0.14). Isso indica que no segundo modelo os pontos dos dados observados 
## estão mais próximos dos valores preditos, do que o primeiro modelo. Analisando os pressupostos, com a tranformação 
## da variável independente, podemos observar que no gráfico dos resíduos versus os valores ajustados, houve uma 
## melhoria significativa em relação ao dados não tranformados. Com isso, podemos afirmar com mais precisão que o modelo 
## é homocedástico, pois os pontos estão distribuídos aleatoriamente, não havendo nenhuma tendência. No gráfico de dispersão 
## do modelo 2 é possível ver que a tranformação level-log resolveu o problema da não-linearidade. O gráfico da normalidade 
## do modelo 2, sugere que a distribuição dos resíduos se aproxima de uma distribuição normal. No teste de shapiro, o p-valor 
## foi de 0.59, ou seja, não significativo (tendo um parâmetro de 0.05). Isso indica que não podemos rejeitar a hipótese nula 
## de que a distribuição dos erros é normal. Analisando a capacidade explicativa do modelo, o modelo 2 apresenta um R2 maior do que o 
## o modelo 1 (0.98>0.53) e com isso maior capacidade de explicar a variação na variável dependente. 

## tranformando o valor de Y quando a não-linearidade é o único problema 

reg3 <- lm((prop^-1.25) ~ time, data = bd)

summary(reg3) 

## Função de logarítimo natural 

bd$lnprop <- (bd$prop^-1.25)

## Gráfico de dispersão com y^1.25

ggplot(data = bd, aes(y = bd$lnprop, x = time)) + geom_point(color = "blue") + 
theme_classic() + geom_smooth(method = "lm", color = "black", se = FALSE) + 
geom_label(aes(x = 0, y = 20), hjust = 0, 
label = paste("Adj R2 = ",signif(summary(reg3)$adj.r.squared, 2),
" \nP =",signif(summary(reg3)$coef[2,4], 2)))


## Gráfico dos resíduos versus valores ajustados com y^-1.25

ggplot(lm(reg3)) + geom_point(aes(x = .fitted, y = .resid)) + geom_abline(slope = 0)

## Gráfico da normalidade dos erros 

shap3 <- shapiro.test(reg3$residuals)

ggplot(data = bd, aes(sample = reg3$residuals)) + 
stat_qq(color = 'blue') + stat_qq_line(color = 'red', lty = 2) + 
geom_label(aes(x = 0, y = 2), hjust = 0, 
label = paste ("shapiro.test =",signif(summary(shap3$p.value)[3], 2)))

---------------------------------------------------------------------------------------------------------------------- 

## Definir diretório 

setwd('C:/Users/usuario/Desktop/lista 9 dados')

## Ler arquivo em txt 

bd2 <- read.delim('shortleaf.txt')

## modelo 4 

reg4 <- lm(Vol ~ Diam, data = bd2)

summary(reg4)

## Gráfico de dispersão modelo 4

ggplot(data = bd2, aes(y = Vol, x = Diam)) + geom_point(color = "blue") + 
theme_classic() + geom_smooth(method = "lm", color = "black", se = FALSE) + 
geom_label(aes(x = 0, y = 145), hjust = 0, 
label = paste("Adj R2 = ",signif(summary(reg4)$adj.r.squared, 5),
" \nP =",signif(summary(reg4)$coef[2,4], 5)))

## ## Gráfico dos resíduos versus valores ajustados modelo 4 

ggplot(lm(reg4)) + geom_point(aes(x = .fitted, y = .resid)) + geom_abline(slope = 0) 

## Gráfico da normalidade modelo 4 

shap4 <- shapiro.test(reg4$residuals)

ggplot(data = bd2, aes(sample = reg4$residuals)) + 
stat_qq(color = 'blue') + stat_qq_line(color = 'red', lty = 2) + 
geom_label(aes(x = 0, y = 40), hjust = 0, 
label = paste ("shapiro.test =",signif(summary(shap4$p.value)[3], 2)))

  ### Um ponto de dados pode ser considerado um "outlier" 
  ### apenas por causa do efeito do ajuste fraco do modelo 

## RMSE modelo 4 

rmse(reg4$residuals)

### logarítimo natural dos diâmetros das árvores 

bd2$lnDiam <- log(bd2$Diam) 

## Modelo 5 level-log 

reg5 <- lm(Vol ~ log(Diam), data = bd2)

summary(reg5)

## Gráfico de dispersão x = lnDiam

ggplot(data = bd2, aes(y = Vol, x = bd2$lnDiam)) + geom_point(color = "blue") + 
theme_classic() + geom_smooth(method = "lm", color = "black", se = FALSE) + 
geom_label(aes(x = 0, y = 25), hjust = 0, 
label = paste("Adj R2 = ",signif(summary(reg5)$adj.r.squared, 5),
" \nP =",signif(summary(reg5)$coef[2,4], 5)))

## Transformar apenas os valores de X não alterou a linearidade. 

## Gráfico dos resíduos versus valores ajustados modelo 5

ggplot(lm(reg5)) + geom_point(aes(x = .fitted, y = .resid)) + geom_abline(slope = 0) 

## O gráfico de resíduos versus ajustes também ainda sugere uma relação não linear ...

## Gráfico da normalidade modelo 5 

shap5 <- shapiro.test(reg5$residuals) 

ggplot(data = bd2, aes(sample = reg5$residuals)) + 
stat_qq(color = 'blue') + stat_qq_line(color = 'red', lty = 2) + 
geom_label(aes(x = 0, y = 50), hjust = 0, 
label = paste ("shapiro.test =",signif(summary(shap5$p.value)[3], 2)))

## pouca melhoria na normalidade do erro 

## tranformar x sozinho não foi suficiente 

## RMSE Modelo 5

rmse(reg5$residuals)

### logarítimo natural do volume das árvores 

bd2$lnVol <- log(bd2$Vol) 

## modelo 6 - Log-log 

reg6 <- lm(log(Vol) ~ log(Diam), data = bd2)

summary(reg6)

## Gráfico de dispersão x = lnDiam e y = lnVol

ggplot(data = bd2, aes(y = bd2$lnVol, x = bd2$lnDiam)) + geom_point(color = "blue") + 
theme_classic() + geom_smooth(method = "lm", color = "black", se = FALSE) + 
geom_label(aes(x = 1.5, y = 5), hjust = 0, 
label = paste("Adj R2 = ",signif(summary(reg6)$adj.r.squared, 5),
" \nP =",signif(summary(reg6)$coef[2,4], 5)))

## O gráfico de resíduos versus ajustes fornece ainda mais evidências de uma relação 
## linear entre lnVol e lnDiam 

## Gráfico dos resíduos versus valores ajustados modelo 6

ggplot(lm(reg6)) + geom_point(aes(x = .fitted, y = .resid)) + geom_abline(slope = 0) 

## Mesmo parecendo que algum "afunilamento" exista, os resíduos negativos seguem a faixa 
## horizontal desejada.

## Gráfico da normalidade modelo 6 

shap6 <- shapiro.test(reg6$residuals)

ggplot(data = bd2, aes(sample = reg6$residuals)) + 
stat_qq(color = 'blue') + stat_qq_line(color = 'red', lty = 2) + 
geom_label(aes(x = 0, y = 0.8), hjust = 0, 
label = paste ("shapiro.test =",signif(summary(shap6$p.value)[3], 2)))

## RMSE Modelo 6

rmse(reg6$residuals)

## b) 

## Pressupostos 

## No modelo 4 é possível observar que o gráfico de dispersão e o gráfico de resíduos versus ajustes,
## sugerem um relacionamento não-linear entre o volume da árvore e o diâmetro. Com isso, não é possível avaliar 
## se a variãncia dos erros são iguais. Além disso, os erros também não parecem normalmente distribuídos.O teste de 
## Shapiro para esse modelo foi significativo, o que sugere que podemos rejeitar a hipótese nula de que a a 
## distribuição dos erros é normal. Transformando apenas o valor de x em log (modelo level-log), vemos que 
## no gráfico de dispersão a relação continua não-linear. O gráfico de resíduos versos valores ajustados 
## também ainda sugere uma relação não-linear. Por último no gráfico da normalidade é possível observar pouca 
## melhoria na normalidade dos resíduos e um teste de Shapiro ainda com um p-valor significativo. Em suma, a 
## transformação apenas dos valores de x não alterou a não-linearidade. Transformando também os valores de resposta
## (modelo log-log), observamos que a relação entre log do diâmetro e o log do volume parece linear. O gráfico dos 
## valores ajusatdos versus resíduos, também fornecem evidências de uma relação linear. Além disso é possível observar 
## que os pontos estão mais "espalhados" com a tranformação, o que indica maiores evidências para um modelo homocedástico. 
## Por último, o gráfico da probabilidade normal melhorou substancialmente, se ajustando melhor a reta. O teste de 
## Shapiro foi não-significativo, o que sugere que podemos não rejeitar a hipótese nula de que os erros estão distribuídos 
## normalmente. Em suma, o modelo log-log funcionou melhor do que o modelo log-level. Existem fortes evidências, de que 
## o relacionamento é linear entre as variáveis, os termos dos erros são independentes e normalmente distribuídos 
## com variações iguais (homocedástico). 

## Resultados do modelo 

## Analisando o ajuste, o modelo 4 apresenta um erro padrão residual de 9.87, o que indica que a média da amostra se desvia 
## em 9.87 das possíveis médias amostrais. RMSE foi de 9.73. Isso indica que o modelo desvia em média 9.73 pontos ao prever a 
## duração da gestação. Os resultados do modelo 6 (transformação log-log), mostram que 1% na variação do diâmetro  dos 
## pinheiros, implica em 2.56% de variação no volume em pés cúbicos, com um p-valor significativo. Comparando os resultados 
## do modelo log-log (modelo 6) com o modelo 4, em relação ao ajuste, esse último apresenta um erro padrão residual maior 
## (0.89), do que o modelo 1 (0.17).Isso significa, que no quarto modelo, a distância média entre os pontos do modelo e 
## a reta de regressão é maior e com isso menos ajustado do que o modelo log-log.  O RMSE do modelo também 4 é maior do que o 
## modelo 6 (9.73 > 0.16). Isso indica que no sexto modelo os pontos dos dados observados estão mais próximos dos valores preditos, 
## do que o quarto modelo. Analisando a capacidade explicativa do modelo, o modelo 6 apresenta um R2 ajustado maior do que o 
## o modelo 4 (0.97>0.89) e com isso maior capacidade de explicar a variação na variável dependente. 
----------------------------------------------------------------------------------------- 

## Definir diretório 

setwd('C:/Users/usuario/Desktop/lista 9 dados')

## Ler arquivo em txt 

load('mammgest.RData')

## Modelo 7 

reg7 <- lm(Gestation ~ Birthwgt, data = mammgest)

summary(reg7)

## Gráfico de dispersão modelo 7 

ggplot(data = mammgest, aes(y = Gestation, x = Birthwgt)) + geom_point(color = "blue") + 
theme_classic() + geom_smooth(method = "lm", color = "black", se = FALSE) + 
geom_label(aes(x = 0, y = 500), hjust = 0, 
label = paste("Adj R2 = ",signif(summary(reg7)$adj.r.squared, 5),
" \nP =",signif(summary(reg7)$coef[2,4], 5)))

## O gráfico sugere que a relação entre comprimento de gestação e peso ao nascer
## é linear, mas que a variância dos termos de erro pode não ser igual

## Gráfico dos resíduos versus valores ajustados modelo 7

ggplot(lm(reg7)) + geom_point(aes(x = .fitted, y = .resid)) + geom_abline(slope = 0) 

## O gráfico fornece evidências de que a variância dos termos dos erros podem não 
## ser iguais 

## Gráfico da normalidade modelo 7 

shap7 <- shapiro.test(reg7$residuals)

ggplot(data = mammgest, aes(sample = reg7$residuals)) + 
stat_qq(color = 'blue') + stat_qq_line(color = 'red', lty = 2) + 
geom_label(aes(x = 0, y = 50), hjust = 1, 
label = paste ("shapiro.test =",signif(summary(shap7$p.value)[3], 2))) 

## RMSE Modelo 7

rmse(reg7$residuals)

### logarítimo natural do período da gestação 

mammgest$lnGest <- log(mammgest$Gestation) 

## Modelo 8 log-linear

reg8 <- lm(log(Gestation) ~ Birthwgt, data = mammgest)

summary(reg8)

## Gráfico de dispersão modelo 8 com Y = lnGestation  

ggplot(data = mammgest, aes(y = mammgest$lnGest, x = Birthwgt)) + geom_point(color = "blue") + 
theme_classic() + geom_smooth(method = "lm", color = "black", se = FALSE) + 
geom_label(aes(x = 0, y = 5), hjust = 0, 
label = paste("Adj R2 = ",signif(summary(reg7)$adj.r.squared, 5),
" \nP =",signif(summary(reg7)$coef[2,4], 5)))

## O gráfico mostra que a tranformação na variável Y tendeu a "espalhar" as gestações menores e tendeu 
## a "trazer" as maiores. 

## Gráfico dos resíduos versus valores ajustados modelo 8

ggplot(lm(reg8)) + geom_point(aes(x = .fitted, y = .resid)) + geom_abline(slope = 0) 

## O gráfico mostra uma melhoria acentuada na expansão dos resíduos

## Gráfico da normalidade modelo 8 

shap8 <- shapiro.test(reg8$residuals)

ggplot(data = mammgest, aes(sample = reg8$residuals)) + 
stat_qq(color = 'blue') + stat_qq_line(color = 'red', lty = 2) + 
geom_label(aes(x = 0, y = 0.5), hjust = 1, 
label = paste ("shapiro.test =",signif(summary(shap8$p.value)[3], 2))) 

## RMSE Modelo 8

rmse(reg8$residuals)

## c) 

## Pressupostos 

## No modelo 7 é possível observar que que o gráfico de dispersão sugere uma relação linear entre o 
## período da gestação e o peso ao nascer. Analisando o gráfico dos resíduos pelos valores ajustados 
## é possíver ver evidências de que a variância dos termos dos erros não é igual. O gráfico da probabilidade 
## do erro mostra evidências dos erros normalmente distribuído, com o p-valor do teste de Shapiro sendo não 
## significativo. Como o  modelo se mostra linear, mas com evidências de ser heterocedástico, o valor de Y foi 
## transformado em log. Avaliando o modelo 8 é possível observar que no gráfico de dispersão, a transformação 
## log-level tendeu a melhorar a relação linear entre as variáveis. O gráfico dos resíduos versus ajustes, 
## mostra uma melhoria acentuada na expansão dos resíduos e com isso fornecendo evidências de um modelo homocedástico. 
## Por último, a transformação não afetou negativamente a normalidade dos termos dos erros. 

## Resultados do modelo 

## Analisando o ajuste, o modelo 7 apresenta um erro padrão residual de 66.09, o que indica que a média da amostra se 
## desvia em 66.09 das possíveis médias amostrais. RMSE foi de 59.78. Isso indica que o modelo desvia em média 59.78 pontos ao 
## prever a duração da gestação.  Os resultados do modelo 8 (transformação log-linear), mostram que a variação do peso do 
## mamífero ao nascer em uma unidade implica em 0.01% de variação na duração da gestação, com um p-valor significativo. Comparando 
## os resultados do modelo 7 e 8, em relação ao ajuste, o primeiro apresenta um erro padrão residual maior (66.09), do que 
## o modelo 8 (0.21). Isso significa, que no sétimo modelo, a distância média entre os pontos do modelo e a reta de regressão 
## é maior e com isso menos ajustado do que o modelo log-level. O RMSE do modelo 7 também  é maior do que o modelo 8 (59.78 > 0.19). 
## Isso indica que no oitavo modelo os pontos dos dados observados estão mais próximos dos valores preditos, do que o sétimo modelo. 
## Em relação a capacidade explicativa, o modelo 7 apresenta um R2 ajustado maior do que o modelo 8 (0.82>0.78), o que indica uma 
## maior capacidade de explicar a variação na variável dependente.

## 4.2 

## a) 

## Uma regressão linear requer que a relação entre a variável dependente e independente seja linear. 
## Em um modelo polinomial, a distribuição dos dados é mais complexa. Na linha de regressão linear é 
## possível capturar os padrões nos dados. Já em uma regressão polinomial, não há uma inclinação única 
## que se aplica a toda variação de Xi. Com isso, não é possível usar o mesmo arcabouço usado em uma regressão 
## linear em uma regressão polinomial. 

## Definir diretório 

setwd('C:/Users/usuario/Desktop/lista 9 dados')

## Ler arquivo em txt 

bluegills <- read.delim('bluegills.txt')

reg9 <- lm(length ~ age, data = bluegills)

summary(reg9) 

## Gráfico de dispersão modelo 9 

age2 <- bluegills$age^2

ggplot(data = bluegills, aes(y = length, x = age2)) + geom_point(color = "blue") + 
theme_classic()

## Gráfico dos resíduos por valores ajustados 

ggplot(lm(reg9)) + geom_point(aes(x = .fitted, y = .resid)) + geom_abline(slope = 0)

## Gráfico normalidade modelo 9 

shap9 <- shapiro.test(reg9$residuals)

ggplot(data = bluegills, aes(sample = reg9$residuals)) + 
stat_qq(color = 'blue') + stat_qq_line(color = 'red', lty = 2) + 
geom_label(aes(x = 0, y = 25), hjust = 1, 
label = paste ("shapiro.test =",signif(summary(shap9$p.value)[3], 2)))

## RMSE modelo 9 

rmse(reg9$residuals)

## Modelo polinomial quadrático 

reg10 <- lm(length ~ poly(age, 2, raw = T), data = bluegills)

summary(reg10)

## Gráfico de dispersão modelo 10 - comparando a curva linear com a polinomial 

ggplot(data = bluegills) + geom_point(aes(x = age, y = length)) + 
geom_line(aes(x = age, y = reg9$fit), color = 'red') + 
geom_line(aes(x = age, y = reg10$fit), color = 'blue') + 
theme(panel.background = element_blank())

## Gráfico dos resíduos por valores ajustados 

ggplot(lm(reg10)) + geom_point(aes(x = .fitted, y = .resid)) + geom_abline(slope = 0)

## Gráfico da normalidade 

shap10 <- shapiro.test(reg10$residuals) 

ggplot(data = bluegills, aes(sample = reg10$residuals)) + 
stat_qq(color = 'blue') + stat_qq_line(color = 'red', lty = 2) + 
geom_label(aes(x = 0, y = 25), hjust = 0, 
label = paste ("shapiro.test =",signif(summary(shap10$p.value)[3], 2)))

## RMSE modelo 10 

rmse(reg10$residuals)

## b) O modelo 9 apresenta um erro padrão residual de 12.51, o que indica  que a média da amostra se 
## desvia em 12.51 das possíveis médias amostrais. RMSE foi de 12.34. Isso indica que o modelo desvia 
## em média 12.34 pontos ao prever o comprimento em mm dos peixes. O gráfico de dispersão do modelo 9, 
## sugere uma tendência positiva nos dados, ou seja, á medida que aumenta a idade do peixe, o comprimento 
## desse tende a aumentar, porém o gráfico mostra evidências de uma tendência não-linear. O gráfico dos 
## resíduos versus os valores ajustados, também sugere um relacionamento não-linear entre as variáveis e 
## mostra evidências de um modelo heterocedástico, pois existe uma tendência no comportamento dos erros. 
## Analisando QQplot é possível ver que os pontos estão próximos a linha o que sugere evidências de que 
## a distribuição dos erros é normal. Com isso, foi adicionado um termo quadrático ao modelo. Analisando 
## o modelo 10, em relação ao ajuste, o erro padrão residual foi menor (10.91), do que o modelo 9 (12.51).
## Isso significa, que no nono modelo, a distância média entre os pontos do modelo e a reta de regressão 
## é maior e com isso menos ajustado. O RMSE do modelo 9 é maior do que o modelo 10 (12.34 > 10.69). 
## Isso indica que no décimo modelo os pontos dos dados observados estão mais próximos dos valores preditos, 
## do que o  modelo 9. No gráfico de dispersão do modelo 10 é possível observar que a regressão quadrática 
## se ajusta melhor aos dados do que o modelo linear. Em relação ao pressuposto da normalidade dos resíduos, 
## no gráfico é possível observar que a distribuição dos resíduos se aproxima de uma distribuição normal. 
## No teste de shapiro, o p-valor foi de 0.056, ou seja, não significativo (tendo um parâmetro de 0.05). 
## Isso indica que os resíduos se aproximam de uma distribuição normal.  

## c) 

## Criando uma nova coluna com os valores ajustado do modelo 10 

bluegills$fitted.values <- reg10$fitted.values 

## Gráfico (2) de dispersão do modelo 10 

ggplot(data = bluegills, aes(y = length, x = age2)) + geom_point(color = "blue") + 
theme_classic() + stat_smooth(method = "lm", formula = y ~ x + I(x^2)) 

## c) O gráfico de dispersão do modelo 9 sugere uma tendência positiva dos dados, ou seja, à medida que a 
## idade do peixe aumenta, o comprimento do peixe tende a aumentar. Porém, chega um momento em que o 
## comprimento decresce quando o peixe chega próximo aos 6 anos, que provavelmente seria a chegada do fim da 
## fase adulta (segundo o wikipédia o peixe bluegills vive de 5 a 8 anos). A tendência do gráfico de dispersão 
## uma relação não-linear. Como a transformação, os resultados da regressão polinomial do modelo 10 mostram, O 
## B1 associado idade do peixe é de 54.04 e o valor de B2 associado a idade^2 é de -4.71. O termo quadrático 
## impacta negativamente a variável dependente de forma crescente a medida que a Vi aumenta. É possível 
## observar essa tendência através dos valores ajustados. Comparando as primeiras observações com as últimas 
## (em ordem decrescente), vemos que a medida que o valor de x aumenta (idade) o valor do Y também aumenta 
## (comprimento). O tamanho do peixe continua aumentando porém cada vez menos, como sugere o formato côncavo 
## do gráfico 

## Primeiras observações 

head(bluegills) ###  Ex: Quando a idade do peixe é 2 o seu comprimento é de 102.84 mm 

## Últimas observações 

tail(bluegills) ###  Ex: Quando a idade do peixe é 5 o seu comprimento é de 165.90 mm 


