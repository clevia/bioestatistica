library(outbreaks)
library(dplyr)
library(readxl)
library(pacman)
library(tidyverse)
library(ISLR)
library(magrittr)

pacman::p_load(dplyr, DescTools, car, MASS, psych, ggplot2, QuantPsyc)

#atividade 2 
#variavel dependente case 
#variavel independente beefcurry

data(Outbreak)
head(Outbreak)

#remoção dos dados perdidos

r <- with(Outbreak, which(Outbreak[,5]==9, arr.ind=TRUE))
Outbreak2 <- Outbreak[-r, ]
View(Outbreak2)
levels(as.factor(Outbreak2$beefcurry))

# definindo case 

case <- (Outbreak2$nausea==1)|(Outbreak2$vomiting==1)|(Outbreak2$abdpain==1)|(Outbreak2$diarrhea==1)
case
beefcurry.eat <- Outbreak2$beefcurry > 0
beefcurry.eat

#criando o modelo para beefcurry 

glm0 <- glm(case ~ beefcurry.eat, family=binomial, data = Outbreak2)
summary(glm0)

coef(summary(glm0))[2,1]
coef(summary(glm0))[2,2]

logistic.display(glm0)

exp(coef(summary(glm0))[2,1])
exp(coef(summary(glm0))[2,1] + c(-1,1) * 1.96 * coef(summary(glm0))[2,2])

tab1(case)
tab1(Outbreak2$nausea)
tab1(Outbreak2$vomiting)
tab1(Outbreak2$abdpain)
tab1(Outbreak2$diarrhea)
table(case, beefcurry.eat)

