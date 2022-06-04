# atividade 4 bioestatistica 

library(dplyr)
library(readxl)
library(pacman)
library(questionr)
library(sjPlot)

pacman::p_load(dplyr, DescTools, car, MASS, psych, ggplot2, QuantPsyc)

stokes2000 <- read_excel("Bioestatística/stokes2000.xlsx")

stokes<- stokes2000[,-5]

head(stokes)
stokes$sexo<- as.factor(stokes$sexo)
stokes$ecg<- as.factor(stokes$ecg)
stokes$idade<- as.factor(stokes$idade)
stokes$dc<- as.factor(stokes$dc)

glimpse(stokes)

plot(x=stokes$idade, y=stokes$dc, yaxt='n', pch=20, col=c("#8a55a8", "#c597e0"))
axis(side=2, at=0:1, labels=0:1, las=1)

mod_sat <- glm(stokes$dc ~ as.factor(1:length(stokes$dc)), family=binomial)
mod_nul <- glm(stokes$dc ~ 1, family=binomial)
mod <- glm(stokes$dc ~ ., data = stokes, family = binomial(link = "logit"))

mod_sat
mod_nul
mod

# comparação de modelos

anova( mod,mod_nul, test="LRT")

# podemos ver que mod foi o melhor modelo

#Teste de Wald
summary(mod)

anova(mod, test = "Chisq")
anova(mod, test = "LRT")

#selecionando as variáveis considerando o AIC

mod <- step(mod, direction = "backward") 

summary(mod)

#vemos que mod foi o melhor pelo modelo considerando o AIC 

# letra b 
#não seria interessante pois se tratam de variáveis qualitativas, não é possível gerar 
#alguma informação de por exemplo "feminino ao quadrado" 

#letra c 

summary(mod)

#D residual deviance 
#n numero de observações
#k numero de variaveis
#D/n-k < 1 = modelo adequado 
#D/n-k < 1 = modelo não é adequado

95.016/(78-4)
#1.284 >1 então o modelo não é adequado, provalmente pelo tamanho do banco de dados

#letra d 

#sim há violação dos pressupostos pois pelo calculo da deviance o resultado foi maior que 1
#sendo assim o pressuposto de adequação do modelo não foi satisfeito

#letra e 

odds.ratio(mod)
plot_model(mod, vline.color = "#129674", sort.est = TRUE, 
           show.values = TRUE, value.offset = .3, col= "blue", title = " razão de chance e intervalos")

#podemos ver no gráfico que as razões de chance são maiores que 1 
#é possivel concluir que não há diferença significativa entre alguém do sexo feminino
#ou masculino de adquirir doenca coronaria

