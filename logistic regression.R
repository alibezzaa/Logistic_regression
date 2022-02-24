install.packages("car")
install.packages("mlogit")

library(car)
library(mlogit)

# With one categorical predictor 

Y = c("Bought", "Bought", "Not bought", "Bought",
      "Not bought", "Bought", "Not bought",
      "Not bought", "Bought", "Bought", "Bought", "Not bought",
      "Not bought", "Bought", "Not bought")

X = c("Male", "Female", "Male", "Male",
      "Male", "Male", "Female", "Female", "Male", "Male",
      "Male", "Female", "Female", "Male", "Female")
buying = cbind.data.frame(X,Y)
write.table(buying, "Purchasing behavior by gender", sep = ",")
buying$Y = relevel(buying$Y, "Not bought", "Bought")
buying$X = relevel(buying$X, "Male", "Female")

model1 = glm(Y~X, data = buying, family = binomial())
summary(model1)
predict(model1, list(X =c("Male", "Female")),
        type="response")
# odds at the baseline level of the predictor
prob_buy_0 = 1/(1+exp(-(1.2528-2.8622*0)))
inverse1 = 1- prob_buy_male
odds1 = prob_buy_male/inverse1
# with one unit change in the predictor
prob_buy_1 = 1/(1+exp(-(1.2528-2.8622*1)))
inverse2 = 1 - prob_buy_1
odds2 = prob_buy_1/inverse2

# odds ratio
ratio = odds2/odds1
ratio
exp(-2.8622)
# as the participant goes from male to female, she doesn't buy much

# With two predictors ##

eelData = read.delim("eel.dat", header = TRUE)
head(eelData)
eelData$Cured = relevel(eelData$Cured, "Not Cured")
eelData$Intervention = relevel(eelData$Intervention, "No Treatment")

# Basic logistic regression

eelModel.1 = glm(Cured~ Intervention, data = eelData, family = binomial())
eelModel.2 = glm(Cured~ Intervention + Duration, data = eelData, family = binomial())

# model 1
summary(eelModel.1)
modelChi = eelModel.1$null.deviance - eelModel.1$deviance
modelChi
chidf = eelModel.1$df.null - eelModel.1$df.residual
chidf
chisq.prob = 1- pchisq(modelChi, chidf)
chisq.prob

R2.hl = modelChi/eelModel.1$null.deviance
R2.hl

exp(eelModel.1$coefficients)# odds ratio
exp(confint(eelModel.1)) # should not cross 1

# model 2
summary(eelModel.2)
anova(eelModel.1, eelModel.2) # unfortunately, no p value

modelChi = eelModel.1$deviance - eelModel.2$deviance
chidf = eelModel.1$df.residual - eelModel.2$df.residual
chisq.prob = 1- pchisq(modelChi, chidf)
modelChi; chidf ; chisq.prob # model2 is no better improvement from model1

# casewise diagnostics
casewise = NULL
casewise$predicted_prob = fitted(eelModel.1)
casewise$stand_resid = rstandard(eelModel.1)
casewise$student_resid = rstudent(eelModel.1)
casewise$dfbeta = dfbeta(eelModel.1)
casewise$dffit = dffits(eelModel.1)
casewise$leverage = hatvalues(eelModel.1)

eelData = cbind.data.frame(eelData, casewise)

# predicted values
head(eelData[, c("Cured", "Intervention", "Duration", "predicted_prob")])

# residuals 

head(eelData[, c("stand_resid", "student_resid", "dfbeta.(Intercept)", "dfbeta.InterventionIntervention", "leverage")])

# example : penalty

penalty = read.delim("penalty.dat", header = TRUE)
head(penalty)

# logistic regression models
model1 = glm(formula = Scored~ Previous + PSWQ, data = penalty, family = binomial())
model2 = glm(formula = Scored ~ Previous + PSWQ + Anxious, data = penalty, family = binomial())

# model1
summary(model1)
model1Chi = model1$null.deviance - model1$deviance
model1Chi
chidf1 = model1$df.null - model1$df.residual
chidf1
chisq.prob1 = 1- pchisq(model1Chi, chidf1)
chisq.prob1

R2_model1 = modelChi/model1$null.deviance
R2_model1

exp(model1$coefficients)# odds ratio
exp(confint(model1)) # should not cross 1
#model 2
summary(model2)
modelChi = model1$deviance - model2$deviance
chidf = model1$df.residual - model2$df.residual
chisq.prob = 1- pchisq(modelChi, chidf)
modelChi; chidf ; chisq.prob # model2 is no better improvement from model1

anova(model1, model2)

# testing multicollinearity

vif(model2)
1/vif(model2)
cor(penalty[, c("PSWQ", "Anxious", "Previous")])

# testing linearity

penalty$log_PSWQ_int = penalty$PSWQ * log(penalty$PSWQ) 
penalty$log_anx_int = penalty$Anxious * log(penalty$Anxious) 
penalty$log_prev_int = penalty$Previous * log(1+penalty$Previous) 
head(penalty)

penaltyTest1 = glm(formula = Scored~ PSWQ + Anxious + Previous + 
                     log_PSWQ_int + log_anx_int + log_prev_int, data = penalty,
                   family = binomial())
summary(penaltyTest1) # interactions are not significant = linearity is met
