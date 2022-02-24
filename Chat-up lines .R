library(mlogit)

chatData = read.delim("Chat-Up Lines.dat", header = TRUE)

head(chatData)
chatData$Gender = relevel(chatData$Gender, ref = 2)
mlChat = mlogit.data(chatData, choice = "Success",
                     shape = "wide")
head(mlChat)
levels(chatData$Success)
chatData$Success = relevel(chatData$Success, ref = 3)
chatModel = mlogit(Success~ 1 | Good_Mate + Funny+ Gender +
                     Sex + Gender:Sex + Funny:Gender  , data = mlChat, reflevel = "No response/Walk Off") 
summary(chatModel)
as.table(exp(chatModel$coefficients))
exp(confint(chatModel))

# testing multicollinearity
library(car)
chatModel1 = glm(Success~ Funny + Good_Mate + Sex + Gender, data = chatData,
                 family = binomial()) 

vif(chatModel1)
1/vif(chatModel1)
cor(chatData[, c("Funny", "Good_Mate", "Sex")])

# testing linearity

mlChat$logFunny = log(mlChat$Funny+ 1)
mlChat$logGood = log(mlChat$Good_Mate + 1)
mlChat$logSex = log(mlChat$Sex + 1)

chattest1 = mlogit(Success~ 1 | Good_Mate + Funny+ Gender +
                     Sex + Funny:logFunny + Good_Mate:logGood +
                    Sex:logSex , data = mlChat, reflevel = "No response/Walk Off") 
summary(chattest1) # some interactions are  significant = linearity not met

# Predicting probability

        