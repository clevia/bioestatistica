#install.packages("ISLR")
#install.packages("epiDisplay")
#install.packages("MASS")
#install.packages("outbreaks")
#install.packages("descriptr")


?glm

library(readxl)
library(tidyverse)
library(ISLR)
library(MASS)
library(magrittr)
library(epiDisplay)
library(outbreaks)
library(descriptr)
tidyverse_update()
library(tidyverse)

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
tab1(Outbreak$age)
tab1(Outbreak$exptime)
tab1(Outbreak$onset)
tab1(Outbreak$beefcurry)


##Dados de Hosmer e Lemeshow 2000  
##Syely Ruiz Giolo - Introdução à análise de dados categóricos com aplicações
library(readxl)

d <- read_excel("C:/Users/55839/Desktop/Elias/UEPB/Bioestatistica/2022/datahosmerlemeshow20002.xlsx")
d
length(d$idade)
glm0 <- glm(d$dcc ~ d$idade, family = binomial)
summary(glm0)
anova(glm0)

logistic.display(glm0)

##Dados https://rpubs.com/fhernanb/deviance_glm
y <- c(1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 
       0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 
       1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0)
weight <- c(2.1, 2.5, 1.2, 1, 3, 2.1, 1.5, 2.2, 1.9, 2.7, 1.1, 2.9, 1.2, 2.1, 
            2.2, 2.5, 1.9, 1.2, 2, 2.9, 2.2, 1.5, 3, 2.4, 1.2, 1.6, 2.3, 2.1, 
            2.6, 2.4, 2.5, 2, 1, 1.4, 2.9, 1.5, 3, 2.9, 2.9, 2.1, 2.8, 2.7, 1, 
            2.9, 1.1, 2.2, 1.3, 1.7, 1.5, 1.7)

plot(x=weight, y, yaxt='n', pch=20)
axis(side=2, at=0:1, labels=0:1, las=1)

##4.1 Saturated and null model
##Modelo saturado e modelo com somente o intercepto 
##To fit the saturated and null model we can use the next code.

mod_sat <- glm(y ~ as.factor(1:length(y)), family=binomial)
mod_nul <- glm(y ~ 1, family=binomial)

##4.2 Proposed model
##Now we are going to fit the proposed model.

mod <- glm(y ~ weight, family=binomial)

##4.3 Deviance manually
##If we wanted to obtain the deviance manually we can use the expression given below and in R we

2*(logLik(mod_sat) - logLik(mod)) # Residual deviance
## 'log Lik.' 35.48677 (df=50)
2*(logLik(mod_sat) - logLik(mod_nul)) # Null deviance
## 'log Lik.' 67.30117 (df=50)


#################################################################################################

library(readxl)
d <- read_excel("C:/Users/55839/Desktop/Elias/UEPB/Bioestatistica/2022/datahosmerlemeshow20002.xlsx")
d
plot(x=d$idade, y=d$dcc, yaxt='n', pch=20)
axis(side=2, at=0:1, labels=0:1, las=1)

mod_sat <- glm(d$dcc ~ as.factor(1:length(d$dcc)), family=binomial)
mod_nul <- glm(d$dcc ~ 1, family=binomial)
mod <- glm(d$dcc ~ d$idade, family=binomial)

mod_sat
mod_nul
mod


summary(mod)##Teste de Wald
anova(mod)
anova(mod, test = "Chisq")
anova(mod, test = "F")##Warning message:In anova.glm(mod, test = "F") : using F test with a 'binomial' family is inappropriate

##Deviances dos modelos com e sem as variáveis independentes de interesse.
Dcom<-2*(logLik(mod_sat) - logLik(mod)) 
Dsem<-2*(logLik(mod_sat) - logLik(mod_nul))

##Teste da razão de verossimilhança
TRV<-Dsem-Dcom
TRV

a<-anova(mod, test = "Chisq")
a
glreg<-a[2,1]
glreg

pvalorC<-pchisq(TRV, glreg, lower.tail = FALSE )
pvalorC

dtab<-c(d[,1],d[,2])
dtab

tab1(dtab$idade)


x<-d$idade
x
xf<-as.factor(x)
xf
xclassf<-levels(xf)
xclassf
xclassn<-as.numeric(xclassf)
xclassn

pcs<-1/(1+exp(-(-5.12300+0.10578*xclassn)))
pcs

or<-pcs/(1-pcs)
or

logito<-log(or)
logito

##razão de chances (odds ratio) para indivíduos com 65 e 25 anos de idade
or[8]/or[1]
##doença coronária entre os indivíduos com 65 anos de idade foi 69 vezes a dos indivíduos com 25 anos.

##Análise de adequação utilizando tabelas de s x 2 entradas
tab<-table(d$idade,d$dcc)
tab
tabaux1<-as.numeric(tab)
tabaux1

tab1x<-tab1(d$idade)
tab1x

aux1<-tab1x$output.table[,1]
aux1
aux2<-aux1[1:length(xclassn)]
aux2
ni<-as.numeric(aux2)
ni
ni2<-tabaux1[1:length(xclassn)]
ni2
ni1<-ni-ni2
ni1
fni1<-ni1/ni
fni1
ei1<-ni*pcs
ei1
ei2<-ni*(1-pcs)
ei2
fi1<-((ni1-ei1)^2)/(ei1)
fi1
s1<-sum(fi1)
s1
fi2<-((ni2-ei2)^2)/(ei2)
fi2
s2<-sum(fi2)
s2
QP<-s1+s2
QP

pvalor<-pchisq(QP,length(xclassn)-2,lower.tail = FALSE)
pvalor

pvalor<-pchisq(0.6,6,lower.tail = FALSE )
pvalor


##coef(glm0)[1] + log10(100)*coef(glm0)[2]
##plot(xclassn, pcs)
##newdata <- data.frame(log10.strep=seq(from=-2, to=4, by=.01))
##newdata
predicted.line <- predict.glm(mod,type="response")
predicted.line
plot(xclassn, fni1, xlab="idade", ylab="E[Y|x]")
lines(d$idade, predicted.line, col="blue")
##axis(side=1, at=20:70)
##title(main="Relationship between mutan streptococci \n and probability of tooth decay", xlab="CFU", ylab="Probability of having decayed teeth")
#Note the use of the '\n' in the command above to separate a long title into two lines.


##########################################################################################

##Exemplo 7.3.4. Tabela 7.22


library(readxl)
d <- read_excel("C:/Users/55839/Desktop/Elias/UEPB/Bioestatistica/2022/stokes2000.xlsx")
d

##dc vs idade
plot(x=d$idade, y=d$dc, yaxt='n', pch=20)
axis(side=2, at=0:1, labels=0:1, las=1)

mod_sat <- glm(d$dc ~ as.factor(1:length(d$dc)), family=binomial)
mod_nul <- glm(d$dc ~ 1, family=binomial)
mod <- glm(d$dc ~ d$idade, family=binomial)

mod_sat
mod_nul
mod

summary(mod)##Teste de Wald
##anova(mod)
anova(mod, test = "Chisq")
anova(mod, test = "F")##Warning message:In anova.glm(mod, test = "F") : using F test with a 'binomial' family is inappropriate

##Deviances dos modelos com e sem as variáveis independentes de interesse.
Dcom<-2*(logLik(mod_sat) - logLik(mod)) 
Dsem<-2*(logLik(mod_sat) - logLik(mod_nul))

##Teste da razão de verossimilhança
TRV<-Dsem-Dcom
TRV

a<-anova(mod, test = "Chisq")
a
glreg<-a[2,1]
glreg

pvalorC<-pchisq(TRV, glreg, lower.tail = FALSE )
pvalorC


x<-d$idade
x
xf<-as.factor(x)
xf
xclassf<-levels(xf)
xclassf
xclassn<-as.numeric(xclassf)
xclassn

sbetas<-summary(mod)
sbetas
beta0<-sbetas$coefficients[1]
beta0
beta1<-sbetas$coefficients[2]
beta1

pcs<-1/(1+exp(-(beta0+beta1*xclassn)))
pcs

or<-pcs/(1-pcs)
or

logito<-log(or)
logito

##razão de chances (odds ratio) para indivíduos com 60 e 28 anos de idade
or[30]/or[1]
##doença coronária entre os indivíduos com 60 anos de idade foi 13 vezes a dos indivíduos com 28 anos.

##Análise de adequação utilizando tabelas de s x 2 entradas
tab<-table(d$idade,d$dc)
tab
tabaux1<-as.numeric(tab)
tabaux1

tab1x<-tab1(d$idade)
tab1x

aux1<-tab1x$output.table[,1]
aux1
aux2<-aux1[1:length(xclassn)]
aux2
ni<-as.numeric(aux2)
ni
ni2<-tabaux1[1:length(xclassn)]
ni2
ni1<-ni-ni2
ni1
ei1<-ni*pcs
ei1
ei2<-ni*(1-pcs)
ei2
fi1<-((ni1-ei1)^2)/(ei1)
fi1
s1<-sum(fi1)
s1
fi2<-((ni2-ei2)^2)/(ei2)
fi2
s2<-sum(fi2)
s2
QP<-s1+s2
QP

pvalor<-pchisq(QP,length(xclassn)-2,lower.tail = FALSE)
pvalor
pvalor<-pchisq(0.6,6,lower.tail = FALSE )
pvalor


ds_freq_table(d,idade, 5)
ds_freq_table(d,idade, 6)
ds_freq_table(d,idade, 7)
ds_freq_table(d,idade, 8)



##dc vs sexo
plot(x=d$sexo, y=d$dc, yaxt='n', pch=20)
axis(side=2, at=0:1, labels=0:1, las=1)

mod_sat <- glm(d$dc ~ as.factor(1:length(d$dc)), family=binomial)
mod_nul <- glm(d$dc ~ 1, family=binomial)
mod <- glm(d$dc ~ d$sexo, family=binomial)

summary(mod)##Teste de Wald
##anova(mod)
anova(mod, test = "Chisq")

################################################################################################
##Estudo sobre doença coronária arterial pag. 135
##Livro da R. G. Suely - Introdução à análise de dados categóricos com aplicações.


d <- read_excel("C:/Users/55839/Desktop/Elias/UEPB/Bioestatistica/2022/stokes20002.xlsx")
d

##dc vs idade
##plot(x=, y=d$dc, yaxt='n', pch=20)
##axis(side=2, at=0:1, labels=0:1, las=1)

mod_sat <- glm(d$dc ~ as.factor(1:length(d$dc)), family=binomial)
mod_nul <- glm(d$dc ~ 1, family=binomial)
mod_full<- glm(d$dc ~ d$sexo+d$ecg+(d$sexo)*(d$ecg), family=binomial)
mod_sex_ecg<-glm(d$dc ~ d$sexo+d$ecg, family=binomial)
#mod_sex <- glm(d$dc ~ d$sexo, family=binomial)
#mod_ecg <- glm(d$dc ~ d$ecg, family=binomial)

summary(mod_full)
anova(mod_full, test = "Chisq")
AIC(mod_full)

summary(mod_sex_ecg)
anova(mod_sex_ecg, test = "Chisq")
AIC(mod_sex_ecg)

##Análise de adequação

tab<-table(d$ecg,d$sexo,d$dc)
tab
tabaux1<-as.numeric(tab)
tabaux1

binsexo<-c(0,0,1,1)
binsexo

binecg<-c(0,1,0,1)
binecg


length(tabaux1)
len<-length(tabaux1)/2
len

ni2<-tabaux1[1:len]
ni2
ni1<-tabaux1[(len+1):length(tabaux1)]
ni1
ni<-ni1+ni2
ni

pcs<-1/(1+exp(-(coef(mod_sex_ecg)[1]+coef(mod_sex_ecg)[2]*binsexo+coef(mod_sex_ecg)[3]*binecg)))
pcs

ei1<-ni*pcs
ei1
ei2<-ni*(1-pcs)
ei2
fi1<-((ni1-ei1)^2)/(ei1)
fi1
s1<-sum(fi1)
s1
fi2<-((ni2-ei2)^2)/(ei2)
fi2
s2<-sum(fi2)
s2
QP<-s1+s2
QP

pvalor<-pchisq(QP,1,lower.tail = FALSE)
pvalor

##Obtendo as E[Y|x]
px<-ni1/ni
px

###Resíduo de Pearson
ci<-(ni1-ei1)/(sqrt(ei1*(1-pcs)))
ci
ci2<-ci^2
ci2
QP<-sum(ci2)
pvalor<-pchisq(QP,1,lower.tail = FALSE)
pvalor

###Resíduo deviance

cdi<-((ni1-ei1)/(sqrt((ni1-ei1)^2)))
cdi

di<-cdi*(2*ni1*log(ni1/ei1)+2*(ni-ni1)*log((ni-ni1)/(ni-ei1)))^(1/2)
di  
di2<-di^2
di2
QL<-sum(di2)
QL
pvalor<-pchisq(QL,1,lower.tail = FALSE)
pvalor
