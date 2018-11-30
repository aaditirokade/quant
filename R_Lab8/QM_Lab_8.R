#####  R Lab 8: Multivariate Regression -- Hospital Infection Data  ####
install.packages("tidyverse")
install.packages("GGally")
install.packages("psych")
install.packages("car")
install.packages("QuantPsyc")

library(GGally)
library(psych)
library(QuantPsyc)
library(car)
library(tidyverse)


hospinf <- read_csv("hospital_infct.csv")

# Basic Statistis of our variables
describe(hospinf[2:12]) # we don't care about 'ID'

# Correlation Matrix 
round(cor(hospinf[2:12]),2)

# Correlation and Scatterplot
ggpairs(hospinf, columns = 2:12)


model_guess <- lm(InfctRsk ~ Stay + Age + Culture + Xray + 
                    Beds + MedSchool + Region + Census + 
                    Nurses + Facilities, data = hospinf)

summary(model_guess)


# Create your model
model1 <- lm(InfctRsk ~ Xray, data = hospinf)
summary(model1)


model2 <- lm(InfctRsk ~ Xray + Culture, data = hospinf)
summary(model2)

model3 <- lm(InfctRsk ~ Xray + Culture + Facilities, data = hospinf)
summary(model3)

model4 <- lm(InfctRsk ~ Xray + Culture + Facilities + Stay, data = hospinf)
summary(model4)


# Final Model
model5 <- lm(InfctRsk ~ Xray + Culture + Facilities + Stay + Region, data = hospinf)
summary(model5)

# Additional Model
model6 <- lm(InfctRsk ~ Culture + Stay , data = hospinf)
summary(model5)


# VIF for multicollinearity
vif(model5)

# Durbin - Watson autocorrelation test
durbinWatsonTest(model5)

# Beta's
lm.beta(model5)

# AIC
# AIC
AIC(model_guess)
AIC(model1) 
AIC(model2) 
AIC(model3) 
AIC(model4) 
AIC(model5)
AIC(model6) 

