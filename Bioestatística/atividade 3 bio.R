#atividade 3 bioestatistica 


library(dplyr)
library(readxl)
library(pacman)

pacman::p_load(dplyr, DescTools, car, MASS, psych, ggplot2, QuantPsyc)

stokes2000 <- read_excel("Bioestatística/stokes2000.xlsx")

stokes<- stokes2000[,-5]
glimpse(stokes)
head(stokes)
stokes$sexo<- as.factor(stokes$sexo)
stokes$ecg<- as.factor(stokes$ecg)
stokes$idade<- as.factor(stokes$idade)
stokes$dc<- as.factor(stokes$dc)

glimpse(stokes)

levels(stokes$sexo)
levels(stokes$ecg)
levels(stokes$idade)

#1)letra A


# para sexo

glm0 <- glm(stokes$dc ~ stokes$sexo, family=binomial, data=stokes)

summary(glm0)

#para idade 

glm1 <- glm(stokes$dc ~ stokes$idade, family=binomial, data=stokes)

summary(glm1)

#para ecg

glm2 <- glm(stokes$dc ~ stokes$ecg, family=binomial, data=stokes)

summary(glm2)

#letra b

#D residual deviance 
#n numero de observações
#k numero de variaveis
#D/n-k < 1 = modelo adequado 
#D/n-k > 1 = modelo não é adequado

# deviance glm0 - sexo
#D/n-k < 1 = modelo adequado
101.84/(78-4)

# o modelo glm0 não é adequado aos dados 

# deviance glm1 - idade

#D residual deviance 
#n numero de observações
#k numero de variaveis
#D/n-k < 1 = modelo adequado 
#D/n-k > 1 = modelo não é adequado
62.651/(78-4) 

# o modelo glm1 é adequado aos dados 


# deviance glm2 - ecg
#D residual deviance 
#n numero de observações
#k numero de variaveis
#D/n-k < 1 = modelo adequado 
#D/n-k > 1 = modelo não é adequado
100.55/(78-4) 

# o modelo glm2 não é adequado aos dados

#razão de chance

#glm0 - sexo

exp(-0.5596) -> odds
odds
odds/(1+odds) -> prob
prob

# há uma chance de 0.36364 ou 36% de probabilidade de um indivíduo do sexo feminino 
#tenha doença coronaria

#glm1 - idade 
exp( 2.303) -> odds
odds
odds/(1+odds) -> prob
prob

#temos 0.9091252  de chances para um individuo entre 60 e 28 anos

#glm2 - ecg 

exp(-0.5596) -> odds
odds
odds/(1+odds) -> prob
prob

#há uma chance de 0.36364 ou 36% de probabilidade de um indivíduo 2 com 0
#tenha doença coronaria




