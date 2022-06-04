#install.packages("ISLR")
install.packages("epiDisplay")
#install.packages("MASS")
install.packages("outbreaks")

?glm

#library(ISLR)
#library(MASS)
library(epiDisplay)
library(outbreaks)

p <- seq(from=0, to=1, by=.01)
p
odds <- p/(1-p)
odds
plot(log(odds), p, type="l", col="blue", ylab="Probability", main="relação entre odds e probabilidade", las=1)
abline(h=.5)
abline(v=0) 


#Cárie dentária. O conjunto de dados Decay é um conjunto de dados simples contendo 
#duas variáveis: 'decay', que é binária e 'strep', que é uma variável contínua.
#Decay significa decair: é um vetor numérico indicando a presença de cárie dentária;
#strep vem de estreptococo}: é um vetor numérico que indica o número de unidades
#formadoras de colônias (CFU) de\textit{Streptococcus mutan} na saliva.

#A variável dependente é 'decay', que indica se uma pessoa tem pelo menos um dente 
#cariado (1) ou não (0). A variável de exposição é 'strep', o número de unidades 
#formadoras de colônias (colony forming units (CFU)) de estreptococos, um grupo de 
#bactérias suspeitas de causar cárie dentária.

data(Decay)
?Decay
des(Decay)
Decay

Decay$strep
tab1(Decay$strep)
summary(Decay)

#A prevalência de ter dentes cariados é igual à média da variável 'decay', ou seja, 
#0,63. 
#O gráfico mostra que a grande maioria tem o valor em torno de 150. Como a 
#distribuição natural das bactérias é logarítmica, uma variável transformada é 
#criada e usada como variável independente.

log10.strep <- log10(Decay$strep)
log10.strep

tab1(log10.strep)
summary(log10.strep)

?glm

glm0 <- glm(Decay$decay~log10.strep, family=binomial, data=Decay)
summary(glm0)
#glm0 <- glm(Decay$decay~Decay$strep, family=binomial, data=Decay)
#summary(glm0)

#Pr(>|z|) para 'log10.strep' é o valor P do teste de Wald. Isso testa se o 
#coeficiente, 1,681, é significativamente diferente de 0. Nesse caso, é.


#O intersepto estimado é -2,554. Isso significa que quando log10.strep é 0 
#(ou strep é igual a 1 UFC), o logit de ter pelo menos um dente cariado é -2,55. 
#Podemos então calcular as probabilidades e probabilidades da linha de base.

exp(-2.554) -> odds
odds
odds/(1+odds) -> prob
prob

#Há uma chance de 0,077, ou uma probabilidade de 7,2%, de ter pelo menos um dente 
#cariado se o número de UFC do estreptococo mutan for de 1 UFC.

#O coeficiente de log10.strep é 1,681. Para cada incremento de unidade de 
#log10(strep), ou um incremento de 10 UFC, o logit aumentará em 1,681. Este 
#incremento de logit é constante, mas não o incremento de probabilidade, porque o 
#último não está em uma escala linear. A probabilidade em cada ponto da UFC é 
#calculada substituindo ambos os coeficientes obtidos do modelo. Por exemplo, a 
#100 UFC, a probabilidade é:

coef(glm0)[1] + log10(100)*coef(glm0)[2]
plot(log10.strep, fitted(glm0))
plot(log10.strep, fitted(glm0), xlim = c(-2,4), ylim=c(0,1), xlab=" ", ylab=" ", xaxt="n", las=1)
newdata <- data.frame(log10.strep=seq(from=-2, to=4, by=.01))
newdata
predicted.line <- predict.glm(glm0,newdata,type="response")
predicted.line
lines(newdata$log10.strep, predicted.line, col="blue")
axis(side=1, at=-2:4, labels=as.character(10^(-2:4)))
title(main="Relationship between mutan streptococci \n and probability of tooth decay", xlab="CFU", ylab="Probability of having decayed teeth")
#Note the use of the '\n' in the command above to separate a long title into two lines.


coef(summary(glm0))[2,1]
coef(summary(glm0))[2,2]

logistic.display(glm0)

exp(coef(summary(glm0))[2,1])
exp(coef(summary(glm0))[2,1] + c(-1,1) * 1.96 * coef(summary(glm0))[2,2])

?Outbreak

data(Outbreak)

Outbreak

#Modelamos 'case' como a variável resposta binária e tomamos 'eclair.eat' como 
#a única variável explicativa.

case <- (Outbreak$nausea==1)|(Outbreak$vomiting==1)|(Outbreak$abdpain==1)|(Outbreak$diarrhea==1)
case
eclair.eat <- Outbreak$eclair > 0
eclair.eat

#eclair é um vetor numérico: pedaços de eclair comidos
#80 = comeu mas não lembrava quanto
#90 = informações totalmente perdidas

tab1(case)
tab1(Outbreak$nausea)
tab1(Outbreak$vomiting)
tab1(Outbreak$abdpain)
tab1(Outbreak$diarrhea)
table(case, eclair.eat)

glm0 <- glm(case ~ eclair.eat, family=binomial, data = Outbreak)
summary(glm0)

#A parte acima da tela é na verdade uma matriz do objeto 'coef(summary(glm0))'. O 
#'logistic.display(glm0)' manipula essa matriz e dá origem a uma exibição mais 
#compreensível pela maioria dos epidemiologistas.

coef(summary(glm0))[2,1]
coef(summary(glm0))[2,2]

logistic.display(glm0)

exp(coef(summary(glm0))[2,1])
exp(coef(summary(glm0))[2,1] + c(-1,1) * 1.96 * coef(summary(glm0))[2,2])

#Você pode alterar os valores padrão adicionando o(s) argumento(s) extra(s) no 
#comando.
#logistic.display(glm0, alfa=0.01, decimal=2)

#A razão de chances da regressão logística é derivada da exponenciação da 
#estimativa, ou seja, 23,75 é obtido de:


#Lidando com dados perdidos da variável saltegg

Outbreak$saltegg

casesalt<-cbind(case,Outbreak$saltegg)
casesalt

casesalt1<-casesalt[1:319,]
casesalt1

casesalt2<-casesalt[325:1094,]
casesalt2

csvalores<-rbind(casesalt1,casesalt2)
csvalores


glm3 <- glm(csvalores[,1] ~ csvalores[,2], family = binomial)
summary(glm3)
logistic.display(glm3)


tab1(Outbreak$saltegg)
tab1(Outbreak$exptime)


