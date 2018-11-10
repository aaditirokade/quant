#####  R Lab 6: Measures of Association for Categorical Variables -- 2016 GSS Data  ####
install.packages("tidyverse")
install.packages("oii")
install.packages("DescTools")
library(DescTools)           
library(oii)
library(tidyverse)

# Importing GSS 2016 Data
GSS2016 <- read_csv("GSS_2016.csv")

###################################################################################
################################## Question 1 #####################################
# Whether anti-religionists should be allowed to teach ('colath') by gender ('sex')
###################################################################################

### Before conducting any analysis, let's construct frequency tables for our variables of interest,
### whether anti-religioinsts should be allowed to teach ‘colath’ and gender ‘sex’.

# Create a new data frame selecting only 'colath’ and ‘sex':
col_sex_2016 <- GSS2016 %>% 
  select(sex, colath) %>% 
  filter(!is.na(colath)) %>%
  filter(!is.na(sex)) 

t1_06 <- table(col_sex_2016$colath, col_sex_2016$sex) %>% 
  print()


# Since we have a 2x2 table. This means that Phi, C, and Lambda are 
# valid measures of association. Cramer’s V is not a valid measure 
# because it is only used when the table is rectangular in shape.


oii.xtab(t1_06, col = TRUE, row = TRUE,stats = TRUE, 
         varnames = c("anti-religioinsts teach", "Sex"))


### We see that Lambda cannot be calculated. This is because the mean of 
### the dependent variable (‘colath’) is the same for both males and females. 
### The mode for both is ‘Allowed.’



################################################################
####################### Question 2 #############################
# How close do you feel to blacks ('closeblk') by race ('race')?
################################################################

# Create a new data table selecting only 'closeblk'’ and ‘race':
cblk_race_2016 <- GSS2016 %>% 
  select(closeblk, race) %>% 
  filter(!is.na(closeblk)) %>%
  filter(!is.na(race))

unique(GSS2016$race)
unique(GSS2016$closeblk)


cblk_race_2016$closeblk <- cblk_race_2016$closeblk %>% 
  recode("not at all close" = "1 not at all close",
         "neither one or the other" = "5 neither one or the other",
         "very close" = "9 very close")

t2_06 <- table(cblk_race_2016$closeblk, cblk_race_2016$race) %>% 
  print()


# ‘closeblk’ is an ordinal variable with 9 categories. ‘race’ is a nominal variable 
# with 3 categories. We have a 9x3 table. Meaning that Cramer’s V and Lambda are 
# valid measures of association for this relationship.

oii.xtab(t2_06, col = TRUE, row = TRUE, stats = TRUE, 
         varnames = c("Close to black people", "Race"))



################################################################################
############################## Question 3 ######################################
# What is the relationship between health ('health') and social class ('class')?
################################################################################

# Create a new data table selecting only 'health'’ and ‘class':
hlth_cls_2016 <- GSS2016 %>% 
  select(health, class) %>% 
  filter(!is.na(health)) %>%
  filter(!is.na(class))

unique(hlth_cls_2016$class)
unique(hlth_cls_2016$health)

health_levels <- c("poor", "fair", "good", "excellent")

hlth_cls_2016$health <- factor(hlth_cls_2016$health, levels = health_levels)
  
class_levels <- c("lower class", "working class", "middle class", "upper class")

hlth_cls_2016$class <- factor(hlth_cls_2016$class, levels = class_levels)

t3_06 <- table(hlth_cls_2016$class, hlth_cls_2016$health) %>% 
  print()
options(scipen=999)

oii.xtab(t3_06, col = TRUE, row = TRUE,stats = TRUE, 
         varnames = c("Class", "Health"))

