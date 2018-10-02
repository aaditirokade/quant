#####  R Lab 1: Measures of Central Tendency -- 2016 GSS Data  ####
install.packages("tidyverse")
library(tidyverse)

# Importing GSS 2016 Data
GSS2016 <- read_csv("/Users/aaditirokade/Desktop/Quant/R Lab 1/GSS_2016.csv")

## (1) ## Finding mode for 'hrs1' for all respondents
#Create the mode function
#na.omit and na.exclude: returns the object with observations removed if they
#contain any missing values; differences between omitting and excluding NAs 
#can be seen in some prediction and residual functions
#unique returns a vector, data frame or array like x but with duplicate 
#elements/rows removed.

Mode <- function(x) {
  ux <- na.omit(unique(x))
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}


#mode_hrs_worked <- Mode(GSS2016$hrs1) %>%
mode_hrs_worked <- Mode(GSS2016$hrs1)
print(mode_hrs_worked)


## (2, 3) ##  Finding median and mean for 'hrs1' for all respondents
# na.rm=TRUE to drop the missing value from the dataset
#summary is a generic function used to produce result summaries of the 
#results of various model fitting functions.
  
mean_hrs_worked <- summary(as.numeric(GSS2016$hrs1, na.rm=TRUE))
print(mean_hrs_worked)


## (4) ## Find mean, median, and mode for 'speduc', 'hlthcare', and 'hivtest'

## speduc = spouses highest year school completed ##

# mean and median: summary
speduc_sm <- summary(GSS2016$speduc)
print(speduc_sm)

# mode: Mode
## R is case sensitive
mode_speduc <- Mode(GSS2016$speduc)
print(mode_speduc)


## hlthcare = should the government provide health care for sick ##
unique(GSS2016$hlthcare)
#unique returns a vector, data frame or array like x but with duplicate elements/rows removed.

str(GSS2016$hlthcare)

# changing characters to factors
GSS2016$hlthcare <- recode(GSS2016$hlthcare,
                           "defin should be" = 1,
                           "probab should be" = 2,
                           "prob should not be" = 3,
                           "defin should not be" = 4
                           )

unique(GSS2016$hlthcare) 
#unique returns a vector, data frame or array like x but with duplicate 
#elements/rows removed

str(GSS2016$hlthcare)
count(GSS2016, hlthcare)

# mean and median
hlthcare_sm <- summary(GSS2016$hlthcare) %>%
  print()

# mode
hlthcare_mode <- Mode(GSS2016$hlthcare) %>%
  print()


## hivtest = have you ever been tested for HIV ##

unique(GSS2016$hivtest)
str(GSS2016$hivtest)

GSS2016$hivtest <- recode(GSS2016$hivtest, 
                          "yes" = 1, 
                          "no" = 2
                          )

#Combining or rearranging a factor can be tedious if it has many levels. 
#Recode supports this step by accepting a direct definition of newlevels 
#by oldlevels as argument and adding an "elselevel" option

unique(GSS2016$hivtest)
count(GSS2016, hivtest)


# mean and median
hivtest_sm <- summary(GSS2016$hivtest)
print(hivtest_sm)


# mode
hivtest_mode <- Mode(GSS2016$hivtest)
print(hivtest_mode)


## (5) ## Split data by sex, Male v. Female and find group statistics for 'educ', 'abpoor', and 'madeg'.

## "educ' = respondent's education
## 'abpoor' = do you think that pregnant women should be allowed to have legal abortions if the family has a very low income and cannot afford any more children?
## 'madeg' = respondets's mother's degree


unique(GSS2016$educ)
unique(GSS2016$abpoor)

GSS2016$abpoor <- recode(GSS2016$abpoor, 
                         "yes" = 1,
                         "no" = 2
                          )
unique(GSS2016$madeg)
GSS2016$madeg <- recode(GSS2016$madeg, 
                        "lt high school" = 1,
                        "high school" = 2,
                        "junior college" = 3,
                        "bachelor" = 4,
                        "graduate" = 5
                        )

# Split by sex
female_data <- GSS2016 %>%
  select(sex, educ, abpoor, madeg)
  filter(sex == "female")

male_data <- GSS2016 %>%
  select(sex, educ, abpoor, madeg)
  filter(sex == "male")


# Mean for 'educ'
female_educ_mean <- mean(female_data$educ, na.rm = TRUE)
print(female_educ_mean)

male_educ_mean <- mean(male_data$educ, na.rm = TRUE)
print(male_educ_mean)

# Summary statistics for 'abpoor'
female_abpoor_mode <- Mode(female_data$abpoor)
print(female_abpoor_mode)
male_abpoor_mode <- Mode(male_data$abpoor)
print(male_abpoor_mode)

# Summary statistics for 'madeg'
female_madeg_mid <- summary(female_data$madeg) %>%
  print()
male_madeg_mid <- summary(male_data$madeg) %>%
  print()
