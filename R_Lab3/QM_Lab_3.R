#####  R Lab 3: Descriptive Statistics and Confidence Intervals - 2016  ####

install.packages("tidyverse")
library(tidyverse)
install.packages("psych")
library(psych)


GSS2016 <- read_csv("GSS_2016.csv")


GSS2016$adults <- recode(GSS2016$adults, 
                         "0" = 0,
                         "1" = 1,
                         "2" = 2,
                         "3" = 3, 
                         "4" = 4,
                         "5" = 5,
                         "6" = 6,
                         "7" = 7,
                         "8 or more" = 8
)


#### Question 1: Use the GSS2016 data to get the mean, 
#### 80%, 95%, and 99% confidence interval for the variable "adults" 

# Select adults from our data frame and drop all null values 
gssadults_06 <- GSS2016 %>% 
  select(adults, sex) %>% 
  filter(!is.na(adults)) %>% 
  filter(!is.na(sex))


# Find the mean and Compute the confidence intervals

# 80% Confidence Intervel
gssadults_06 %>% 
  summarise(mean.ad = mean(adults),
            sd.ad = sd(adults),
            n.ad = n()) %>%
  mutate(se.ad = sd.ad / sqrt(n.ad),
         "80%_low.CI" = mean.ad - qt(1 - (0.20 / 2), 
                                     n.ad -1) *se.ad,
         "80%_upp.CI" = mean.ad + qt(1 - (0.20 / 2), 
                                     n.ad -1) *se.ad)

# 95% Confidence Intervel
gssadults_06 %>% 
  summarise(mean.ad = mean(adults),
            sd.ad = sd(adults),
            n.ad = n()) %>%
  mutate(se.ad = sd.ad / sqrt(n.ad),
         "95%_low.CI" = mean.ad - qt(1 - (0.05 / 2), n.ad -1) *se.ad,
         "95%_upp.CI" = mean.ad + qt(1 - (0.05 / 2), n.ad -1) *se.ad)

# 99% Confidence Intervel
gssadults_06 %>% 
  summarise(mean.ad = mean(adults),
            sd.ad = sd(adults),
            n.ad = n()) %>%
  mutate(se.ad = sd.ad / sqrt(n.ad),
         "99%_low.CI" = mean.ad - qt(1 - (0.01 / 2), n.ad -1) *se.ad,
         "99%_upp.CI" = mean.ad + qt(1 - (0.01 / 2), n.ad -1) *se.ad)





#### Question 2: Repeat question 1, but this time split the data by SEX

# 80% Confidence Intervel by `sex'
gssadults_06 %>% 
  group_by(sex) %>% # this output gives us outputs based on `sex'
  summarise(mean.ad = mean(adults),
            sd.ad = sd(adults),
            n.ad = n()) %>%
  mutate(se.ad = sd.ad / sqrt(n.ad),
         "80%_low.CI" = mean.ad - qt(1 - (0.20 / 2), 
                                     n.ad -1) *se.ad,
         "80%_upp.CI" = mean.ad + qt(1 - (0.20 / 2), 
                                     n.ad -1) *se.ad)

# 95% Confidence Intervel by `sex'
gssadults_06 %>% 
  group_by(sex) %>% 
  summarise(mean.ad = mean(adults),
            sd.ad = sd(adults),
            n.ad = n()) %>%
  mutate(se.ad = sd.ad / sqrt(n.ad),
         "95%_low.CI" = mean.ad - qt(1 - (0.05 / 2), n.ad -1) *se.ad,
         "95%_upp.CI" = mean.ad + qt(1 - (0.05 / 2), n.ad -1) *se.ad)

# 99% Confidence Intervel by `sex'
gssadults_06 %>% 
  group_by(sex) %>% 
  summarise(mean.ad = mean(adults),
            sd.ad = sd(adults),
            n.ad = n()) %>%
  mutate(se.ad = sd.ad / sqrt(n.ad),
         "99%_low.CI" = mean.ad - qt(1 - (0.01 / 2), n.ad -1) *se.ad,
         "99%_upp.CI" = mean.ad + qt(1 - (0.01 / 2), n.ad -1) *se.ad)

