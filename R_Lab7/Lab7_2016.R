#####  R Lab 7: Correlation and Regression -- 2016 GSS Data  ####
install.packages("tidyverse")
library(tidyverse)


# Importing GSS 2016 Data
gss2016 <- read_csv("GSS_2016.csv")

### Use the GSS16 dataset and answer the following questions about 
### the relationship between respondent's years of education 
### ('educ') and mother's years of education ('maeduc').


# Create a new data frame selecting 'educ', 'maeduc’, and ‘race':
m_educ_2016 <- gss2016 %>%
  select(educ, maeduc, race) %>%
  filter (!is.na(educ)) %>%
  filter (!is.na(maeduc))


# Scatterplot: Respondent's years of Education vs mother's years of Education.
# geompoint is for scatterplot but there's so many variables that its too much data for that here. 
# jitter concentrates the data
ggplot(m_educ_2016, aes(x = maeduc, y = educ)) +
  geom_jitter(size = 1.5) +
  labs(x = "Mother's Years of Education", y = "Years of Education", 
       title = "Relationship of Education and Mother's Education (2016)")


# 1.	What is the Pearson's r equal to? 
cor.test(m_educ_2016$educ, m_educ_2016$maeduc)

# 2.	What is the y intercept equal to? 
# regression line for 'educ' and 'maeduc'
educ_model_16 <- lm(educ ~ maeduc, data = m_educ_2016)

# you can get just the coefficients of your model
coef(educ_model_16)

# you can also get a full summary
summary(educ_model_16)


# visualizing our model and adding aesthetics
# Scatterplot with regression line 
ggplot(m_educ_2016, aes(x = maeduc, y = educ)) +
  geom_jitter(size=1.5, aes(color = race, shape = race)) +
  labs(x = "Mother's Years of Education", y = "Years of Education", 
       title = "Relationship of Education and Mother's Education (2016)")+
  geom_smooth(method = lm, color = "red")

############################################################
############################################################
############################################################

### Split by 'race'
white_16 <- m_educ_2016 %>%
  filter(race == "white")

black_16 <- m_educ_2016 %>%
  filter(race == "black")

other_16 <- m_educ_2016 %>%
  filter(race == "other")

# Correlation by race
# from this dataset give me column $ for a variable
cor.test(white_16$educ, white_16$maeduc)   # White
cor.test(black_16$educ, black_16$maeduc)   #  Black
cor.test(other_16$educ, other_16$maeduc)   #  Other


# regression models for 'educ' and 'maeduc' by race

# White
white_model_16 <- lm(educ ~ maeduc, data = white_16)

# model summary
summary(white_model_16)

# Black
black_model_16 <- lm(educ ~ maeduc, data = black_16)

# model summary
summary(black_model_16)

# Other
other_model_16 <- lm(educ ~ maeduc, data = other_16) 

# model summary
summary(other_model_16)

############################################################
############################################################
############################################################

# Scatter plots by race

# White
ggplot(white_16, aes(x = maeduc, y = educ)) +
  geom_jitter(size=1.5, color = "royal blue") +
  labs(x = "Mother's Years of Education", y = "Years of Education",  
       title = "White - Education and Mother's Education (2016)") +
  geom_smooth(method = lm, color = "red")

# Black
ggplot(black_16, aes(x = maeduc, y = educ)) +
  geom_jitter(size=1.5, color = "light coral") +
  labs(x = "Mother's Years of Education", y = "Years of Education",  
       title = "Black - Education and Mother's Education (2016)") +
  geom_smooth(method = lm, color = "blue")

# Other
ggplot(other_16, aes(x = maeduc, y = educ)) +
  geom_jitter(size=1.5, color = "green3") +
  labs(x = "Mother's Years of Education", y = "Years of Education",  
       title = "Other - Education and Mother's Education (2016)") +
  geom_smooth(method = lm, color = "red")

