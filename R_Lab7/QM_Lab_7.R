#####  R Lab 7: Correlation and Regression -- 2006 GSS Data  ####
install.packages("tidyverse")

library(tidyverse)


# Importing GSS 2006 Data
GSS2006 <- read_csv("GSS_2006.csv")

### Use the GSS06 dataset and answer the following questions about 
### the relationship between respondent's years of education 
### ('educ') and mother's years of education ('maeduc').


# Create a new data frame selecting only 'maeduc’ and ‘educ':
m_educ_2006 <- GSS2006 %>% 
  select(educ, maeduc, race) %>% 
  filter(!is.na(educ)) %>%
  filter(!is.na(maeduc))


# Scatterplot: Respondent's years of Education vs mother's years of Education.
ggplot(m_educ_2006, aes(x = maeduc, y = educ)) +
  geom_jitter(size=1.5, aes(color = race, shape =race)) +
  labs(x = "Mother's Years of Education", y = "Years of Education", 
       title = "Relationship of Education and Mother's Education (2006)")

#1. what is the Pearson's r equal to
cor.test(m_educ_2006$educ, m_educ_2006$maeduc)

#
coef(educ_model_16)

# Pearson's r 
m_educ_2006 %>% 
  summarise(cor=cor(educ, maeduc))

# Pearson's r with stats
m_educ_2006 %>% 
  do(tidy(cor.test(.$educ, .$maeduc)))

# regression line for 'educ' and 'maeduc'
model_7.1 <- lm(educ ~ maeduc, data = m_educ_2006)

# you can get just the coefficients of your model
tidy(model_7.1)

# you can also get a summary with R-squared and residuals
glance(model_7.1)

# visualizing our model
# Scatterplot with regression line 
ggplot(m_educ_2006, aes(x = maeduc, y = educ)) +
  geom_jitter(size=1.5, aes(color = race, shape =race)) +
  labs(x = "Mother's Years of Education", y = "Years of Education",  
       title = "Relationship With Regression Line (2006)") +
  geom_smooth(method = lm, color = "red")


############################################################
############################################################
############################################################

## Group by 'race' and get Pearson's r
m_educ_2006 %>% 
  group_by(race) %>% 
  summarise(cor=cor(educ, maeduc))

## Pearson's r with stats
m_educ_2006 %>% 
  group_by(race) %>% 
  do(tidy(cor.test(.$educ, .$maeduc)))

## using `broom', we can generate a model that will
## give us stattistics by 'race'.
m_educ_race <- m_educ_2006 %>% group_by(race) %>% 
  do(model_7.2 = lm(educ ~ maeduc, data = .))

## get the coefficients by race in a tidy data_frame
m_educ_race %>% 
  tidy(model_7.2)

## Summary by race
m_educ_race %>% 
  glance(model_7.2)





