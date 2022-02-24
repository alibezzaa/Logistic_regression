donnees = read.csv("sample.csv")

head(donnees)
summary(donnees)

# modèle de base 

model1 = glm(formula = hon~1, data = donnees,
             family = binomial())
summary(model1)

# calcul manuel

table(donnees$hon)
p = 49/200
q  = 1- p
odds = p/q
log(odds)
exp(model1$coefficients)/(exp(model1$coefficients)+1)
p

# modèle avec 1 VI binaire

model2 = glm(formula = hon~female, data = donnees,
             family = binomial())
summary(model2)

## calcul manuel

table(donnees$hon, donnees$female)

# b0
oddsH = 17/74
log(oddsH)

# b1

oddsF = 32/77
ratio = oddsF/oddsH
log(ratio)
exp(model2$coefficients[1]+model2$coefficients[2]*0)/
  (1+exp(model2$coefficients[1]+ model2$coefficients[2]*0))
17/91
