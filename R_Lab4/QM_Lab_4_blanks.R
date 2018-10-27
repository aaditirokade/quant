#####  R Lab 4: Analysis of Variance -- 2016 GSS Data  ####

install.packages("tidyverse")
library(tidyverse)

# Importing GSS 2016 Data
GSS2016 <- read_csv("GSS_2016.csv")

### Before conducting any analysis, we always want to construct frequency tables for
### our variables of interest, ‘degree’ and ‘childs’.

# create a new data frame selecting only 'degree' and 'childs'
deg_child_06 <- GSS2016 %>%
  select(degree, childs) %>% 
  filter(!is.na(degree)) %>% 
  filter(!is.na(childs))


# Frequency table for 'degree'
count(deg_child_06, degree) %>% 
  mutate(percent = n/sum(n)*100) %>%
  arrange(desc(percent)) %>% 
  mutate(cumulative_prc = cumsum(percent))

#unique responses
unique(deg_child_06$childs)

#get rid of non-numeric data
deg_child_06$childs <- recode(deg_child_06$childs, 
                        "0" = 0,
                        "1" = 1,
                        "2" = 2,
                        "3" = 3, 
                        "4" = 4,
                        "5" = 5,
                        "6" = 6,
                        "7" = 7,
                        "eight or more" = 8
)

# Frequency table for 'childs'
count(deg_child_06, childs) %>% 
  mutate(percent = n/sum(n)*100) %>% 
  arrange(desc(percent)) %>% 
  mutate(cumulative_prc = cumsum(percent))
  
# Table for 'degree' and 'childs'
table(deg_child_06$degree, deg_child_06$childs)


# Descriptives Table for ‘childs’ by ‘degree'.
deg_child_06 %>% 
  group_by(degree) %>% 
  summarize(count= n(), mean = mean(childs), std.dev = sd(childs)) %>%
  arrange(desc(mean)) %>% 
  print()

### Based on these tables, we know that our independent variable, ‘degree’, is
### measured at the ordinal level and has more than two categories. Our dependent
### variable, ‘childs’, is measured at the interval-ratio level. Therefore, we can
### compute an ANOVA(Abalysis of variance) model using these two variables. 
??highlight
# ANOVA test
model <- aov(childs ~ degree, data = deg_child_06)

summary(model) # getting the summary of our ANOVA model
