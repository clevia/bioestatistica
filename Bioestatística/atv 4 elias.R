library(dplyr)
library(readxl)
library(pacman)
library(questionr)
library(sjPlot)
library(xlsx)

stokes <- read_excel("Bioestatística/stokes2000.xlsx", 1, header=TRUE, startRow=1)

stokes$sexo<- as.factor(stokes$sexo)
stokes$ecg<- as.factor(stokes$ecg)
stokes$dc<- as.factor(stokes$dc)

mod_sat <- glm(stokes$dc ~ as.factor(1:length(stokes$dc)), family=binomial)
mod_nul <- glm(stokes$dc ~ 1, family=binomial)
mod <- glm(stokes$dc ~ ., data = stokes, family = binomial(link = "logit"))

mod_sat
mod_nul
mod

# comparação de modelos

anova( mod,mod_nul, test="LRT")

#Teste de Wald
summary(mod)

anova(mod, test = "Chisq")
anova(mod, test = "LRT")

#selecionando as variáveis considerando o AIC

mod <- step(mod, direction = "backward") 

summary(mod)

# letra b 
#não seria certo pois se tratam de variáveis de especto qualitativo

#letra c 

summary(mod)
95.016/(78-4)
#1.284 >1 então o modelo não é adequado
#letra d 

#não atende os pressupostos pois pelo calculo da deviance o resultado foi maior que 1

#letra e 

odds.ratio(mod)

#vemos que o odds ratio foi maior que 1

