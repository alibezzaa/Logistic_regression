setwd("~/Desktop/analyse de données/My Course/Logistic regression/Excel_Chapter_Tables_Input_Data")

library(car) # vif
library(ggplot2) # graphique
library(tidyverse) # modifier la bdd

# importer les données

myData = read.csv("Table 18.6.csv", sep = ";")
myData = myData[,-1]

# inspecter les données 

head(myData)
summary(myData)
names(myData) = c("Fidelite", "Marque", "Produit", "Experience_achat")


# représentation graphique 

nuage = ggplot(myData, aes(Marque, Fidelite))+ geom_point()
nuage

# estimer et évaluer le modèle

model0 = glm(Fidelite~1, data = myData, family = binomial())
summary(model0)

model1 = glm(Fidelite ~ .,
             family = binomial(), data = myData)
summary(model1)

# tester l'hypothèse de linéarité 

prob = predict(model1, type = "response")
predictors = colnames(myData[2:4])
mydata2 = myData[2:4] %>% mutate(logit = log(prob/(1- prob))) %>% 
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata2, aes(logit, predictor.value)) +
  geom_point(size = 0.5, alpha = 0.5)+ geom_smooth(method = "loess")+
  facet_wrap(~ predictors, scales = "free_y")

# la multicollinéarité

vif(model1)

# est-ce que le modèle est significatif?

modelChi = model1$null.deviance - model1$deviance
modeldl = model1$df.null - model1$df.residual
modelp = 1 - pchisq(modelChi, modeldl)
modelChi ; modeldl ; modelp

# inspecter l'influence des variables indépendantes

exp(model1$coefficients)
exp(Confint(model1))

# évaluer la qualité du modèle (source : Discovering statistics using R, auteur : Andy field)

logisticPseudoR2s <- function(LogModel){
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev/ nullDev
  R.cs <- 1 - exp(-(nullDev - dev) /modelN)
  R.n <- R.cs / (1 - (exp(-(nullDev/modelN))))
  cat("R^2 pour la régression logistique\n")
  cat("Hosmer et Lemeshow R^2 ", round(R.l, 3), "\n")
  cat("Cox et Snell R^2       ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2          ", round(R.n, 3), "\n")
}
logisticPseudoR2s(model1)

# diagnostic des résidus et des valeurs aberrantes

myData$residus = rstandard(model1) 
myData$large_resid = myData$residus > 2 | myData$residus < -2 # résidus larges

myData$leverage = hatvalues(model1) # seuil = 3*(k+1)/n = 0.4

hist(myData$residus) # normalité
