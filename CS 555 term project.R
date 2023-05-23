
library(dplyr)
library(tidyr)
library(ggplot2)
library(stats)
library(car)


airbnb <- read.csv('Aemf1.csv')
airbnb <- airbnb[airbnb$City =='Amsterdam',]



set.seed(123) 
sample_indices <- sample(nrow(airbnb), size = 1000, replace = FALSE)
airbnb <- airbnb[sample_indices, ]



airbnb <- airbnb[,-c(1,5,6,9,10)]



airbnb$Weekday <- ifelse(airbnb$Day=='Weekday', 1, 0)
airbnb$Private <- ifelse(airbnb$Room.Type=='Private room', 1,0)
airbnb$Super_host <- ifelse(airbnb$Superhost=='True',1,0)



airbnb <- airbnb[,-c(2,3,5)]


# Summary of Guest Satisfaction

GuestWeekday <- airbnb$Guest.Satisfaction[airbnb$Weekday==1]
GuestWeekend <- airbnb$Guest.Satisfaction[airbnb$Weekday==0]
summary(GuestWeekday)
summary(GuestWeekend)

# simple t-test of Weekday and Weekend with 95% CI(Guest Satisfaction)

t.test(GuestWeekday, mu=96, alternative = 'two.sided',conf.level = 0.95)

t.test(GuestWeekend, mu=96, alternative = 'two.sided',conf.level = 0.95)

# two sample t-test of Weekday and Weekend (Guest Satisfaction)

t.test(GuestWeekday, GuestWeekend, alternative = 'two.sided',conf.level = 0.95)

# Summary of Cleaning Rating

CleanWeekday <- airbnb$Cleanliness.Rating[airbnb$Weekday==1]
CleanWeekend <- airbnb$Cleanliness.Rating[airbnb$Weekday==0]
summary(CleanWeekday)
summary(CleanWeekend)

# simple t-test of Weekday and Weekend with 95% CI(Cleaning Rating)

t.test(CleanWeekday, mu=10, alternative = 'two.sided',conf.level = 0.95)

t.test(CleanWeekend, mu=10, alternative = 'two.sided',conf.level = 0.95)

# two sample t-test of Weekday and Weekend (Cleaning Rating)

t.test(CleanWeekday, CleanWeekend, alternative = 'two.sided',conf.level = 0.95)

# simple linear regression of price and guest

fit_guest <- lm(airbnb$Price~airbnb$Guest.Satisfaction, data = airbnb)
summary(fit_guest)

res_guest <- residuals(fit_guest)
pred_guest <- predict(fit_guest)
plot(x = pred_guest, y = res_guest, 
     xlab = "Predicted Value", ylab = "Residuals Value",
     main = "Residual Plot")

anova(fit_guest)

# simple linear regression of price and cleaness

fit_clean <- lm(airbnb$Price~airbnb$Cleanliness.Rating, data = airbnb)
summary(fit_clean)

res_clean <- residuals(fit_clean)
pred_clean <- predict(fit_clean)
plot(x = pred_clean, y = res_clean, 
     xlab = "Predicted Value", ylab = "Residuals Value",
     main = "Residual Plot")


anova(fit_clean)

# Multilinear Regression(Guest Satisfaction) 

model <- lm(airbnb$Guest.Satisfaction ~ ., data = airbnb)
summary(model)


anova(model)


Anova(model, type = 3)

confint(model,level = 0.95)


glm_model <- glm(airbnb$Guest.Satisfaction ~ .,data = airbnb)

summary(glm_model)

anova(glm_model)

# Mutiple Linear Regression(Price)

model2 <- lm(airbnb$Price~., data = airbnb)
summary(model2)

anova(model2)

confint(model2)


Price_glm <- glm(airbnb$Price~.,data = airbnb)
summary(Price_glm)

anova(Price_glm)

Anova(model2, type = 3)


str(airbnb)


