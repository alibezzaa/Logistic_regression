p = seq(from= 0.01, to= 0.9, by = 0.05)
q = 1-p
odds = p/q
log(odds)
plot(odds, log(odds), type = "l")

# dataset examples

# Logistic regression with no predictor variables

model1 = glm(formula = hon~1, data = sample, family = binomial())
summary(model1)

# p is hon = 1
table(sample$hon)
p = 49/200
q = 1 - p
b0 = log(p/q)
b0

# from log of the odds to p

exp(b0)/(1+exp(b0))
p

# Logistic regression with a single dichotomous predictor variables

model2 = glm(hon~ female, data = sample, 
             family = binomial())
summary(model2)

# manually

table(sample$hon, sample$female)
maleOdds = (17/91)/(74/91)
femaleOdds  = (32/109)/(77/109)
ratio = femaleOdds/maleOdds
# the odds for females are 81% higher than for males

# interpretation of coefficients

log(maleOdds)
log(ratio)
exp(model2$coefficients)
maleOdds
ratio
exp(model2$coefficients)/(1+exp(model2$coefficients))
# Logistic regression with a single continuous predictor variable

model3 = glm(hon~ math, data = sample,
             family = binomial())
summary(model3)
exp(model3$coefficients)

# interpretation of coefficients

hist(sample$math)
# if math value = 60 and then 61

-9.79394 + 0.15634*61 -
(-9.79394 + 0.15634*60)
# the beta means every one unit change in the coefficient
# changes the log odds by 0.15634 
# or exp(0.15634) = 1.17, 17% increase in odds





