
library(car)

# example is from the book "Marketing Research : An applied orientation"
Loyalty = read.csv2("Table 18.6.csv")

# we first use an OLS to illustrate the limitations 

olsModel = lm(Loyalty~ Brand + Product + Shopping, data = Loyalty)
summary(olsModel)# for low values of the predictors (1), the probability is negative
# they are greater than 1 for high value of the predictors (7)

# Now we use LR
LoyalModel = glm(formula = Loyalty ~ Brand + Product + Shopping, data = Loyalty,
                 family = binomial())
summary(LoyalModel)

# calculate the significance of the model

modelChi = LoyalModel$null.deviance - LoyalModel$deviance
modelChi
chidf = LoyalModel$df.null - LoyalModel$df.residual 
chiProb = 1- pchisq(modelChi, chidf)
modelChi ; chidf ; chiProb

# Rsquares

logisticPseudoR2s <- function(LogModel){
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev/ nullDev
  R.cs <- 1 - exp(-(nullDev - dev) /modelN)
  R.n <- R.cs / (1 - (exp(-(nullDev/modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2 ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2       ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2          ", round(R.n, 3), "\n")
}

logisticPseudoR2s(LoyalModel)

# odds ratio and CI

exp(LoyalModel$coefficients)# odds ratio
exp(confint(LoyalModel)) # should not cross 1

# testing multicollinearity

vif(LoyalModel)

# testing linearity

Loyalty$BrandLog = log(Loyalty$Brand + 1)
Loyalty$ProductLog = log(Loyalty$Product + 1)
Loyalty$ShoppingLog = log(Loyalty$Shopping + 1)

LoyalModel2 = glm(formula = Loyalty ~ Brand + Product + Shopping + Brand:BrandLog
                  + Product:ProductLog + Shopping:ShoppingLog, 
                  data = Loyalty, family = binomial())
summary(LoyalModel2)# linearity is OK!

## casewise diagnostics

# % of large residuals 
Loyalty$standardized.residuals = rstandard(LoyalModel)
sum(Loyalty$standardized.residuals >2 | Loyalty$standardized.residuals < -2)

# shape of distribution of studentized residuals
Loyalty$studentized.residuals = rstudent(LoyalModel)
hist(Loyalty$studentized.residuals)

# leverage cases
Loyalty$leverage = hatvalues(LoyalModel)
# check for values higher than 3 times the average leverage = (n predictors +1)/n 
Loyalty$leverage

# difference in betas greater than 1
Loyalty$DFBeta = dfbeta(LoyalModel)
Loyalty[, c( "leverage", "DFBeta")]


