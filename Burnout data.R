BurnoutData = read.delim("Burnout.dat", header = TRUE)
BurnoutData$burnout = relevel(BurnoutData$burnout, 2)
summary(BurnoutData)
model1 = glm(burnout ~ loc + cope, data = BurnoutData, 
             family =  binomial())
model2 = glm(burnout~ loc + cope + teaching + research + pastoral, 
             data = BurnoutData, family = binomial())
summary(model1)
summary(model2)
model1Chi = model1$null.deviance - model1$deviance
model1df  = model1$df.null - model1$df.residual
model1_prob = 1 - pchisq(model1Chi, model1df)
model1Chi ; model1df ; model1_prob

# Function to compute R^2's
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

logisticPseudoR2s(model1)

# odds ratio and CI

exp(model1$coefficients)# odds ratio
exp(confint(model1)) # should not cross 1

# comparing models

difChi = model1$deviance - model2$deviance
difdf = model1$df.residual - model2$df.residual
diff.prob = 1 - pchisq(difChi, difdf )
difChi ; difdf ; diff.prob

anova(model1 , model2)

logisticPseudoR2s(model2)
exp(model2$coefficients)
exp(confint(model2))

