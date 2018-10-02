#####  R Lab 1: Measures of Central Tendency -- 2006 GSS Data  ####
install.packages("tidyverse")
library(tidyverse)

# Importing GSS 2006 Data
GSS2006 <- read_csv("/Users/aaditirokade/Desktop/Quant/R Lab 1/GSS_2006.csv")

## (1) ## Finding mode for 'hrs1' for all respondents
# Create mode function
Mode <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}

mode_hrs_worked <- Mode(GSS2006$hrs1) %>%
  print()

## (2, 3) ##  Finding median and mean for 'hrs1' for all respondents
mean_hrs_worked <- summary(as.numeric(GSS2006$hrs1, na.rm=TRUE)) %>%
  print()

## (4) ## Find mean, median, and mode for 'speduc', 'hlthcare', and 'hivtest'

## speduc = spouses highest year school completed ##

# mean and median
speduc_sm <- summary(GSS2006$speduc) %>%
  print()

# mode
mode_speduc <- Mode(GSS2006$speduc) %>%
  print()

## hlthcare = should the government provide health care for sick ##
unique(GSS2006$hlthcare)
str(GSS2006$hlthcare)

# changing characters to factors
GSS2006$hlthcare <- recode(GSS2006$hlthcare,
                           "defin should be" = 1,
                           "probab should be" = 2,
                           "prob should not be" = 3,
                           "defin should not be" = 4
                           )

unique(GSS2006$hlthcare)
str(GSS2006$hlthcare)
count(GSS2006, hlthcare)

# mean and median
hlthcare_sm <- summary(GSS2006$hlthcare) %>%
  print()

# mode
hlthcare_mode <- Mode(GSS2006$hlthcare) %>%
  print()


## hivtest = have you ever been tested for HIV ##

unique(GSS2006$hivtest)

str(GSS2006$hivtest)

GSS2006$hivtest <- recode(GSS2006$hivtest, 
                          "yes" = 1,
                          "no" = 2
                          )

unique(GSS2006$hivtest)

count(GSS2006, hivtest)


# mean and median
hivtest_sm <- summary(GSS2006$hivtest) %>%
  print()


# mode
hivtest_mode <- Mode(GSS2006$hivtest) %>%
  print()


## (5) ## Split data by sex, Male v. Female and find group statistics for 'educ', 'abpoor', and 'madeg'.

## "educ' = respondent's education
## 'abpoor' = do you think that pregnant women should be allowed to have legal abortions if the family has a very low income and cannot afford any more children?
## 'madeg' = respondets's mother's degree


unique(GSS2006$educ)
unique(GSS2006$abpoor)

GSS2006$abpoor <- recode(GSS2006$abpoor, 
                         "yes" = 1,
                         "no" = 2
                          )
unique(GSS2006$madeg)
GSS2006$madeg <- recode(GSS2006$madeg, 
                        "lt high school" = 1,
                        "high school" = 2,
                        "junior college" = 3,
                        "bachelor" = 4,
                        "graduate" = 5
                        )

# Split by sex
female_data <- GSS2006 %>%
  select(sex, educ, abpoor, madeg) %>%
  filter(sex == "female")

male_data <- GSS2006 %>%
  select(sex, educ, abpoor, madeg) %>%
  filter(sex == "male")


# Mean for 'educ'
female_educ_mean <- mean(female_data$educ, na.rm = TRUE) %>%
  print()

male_educ_mean <- mean(male_data$educ, na.rm = TRUE) %>%
  print()

# Summary statistics for 'abpoor'
female_abpoor_mode <- Mode(female_data$abpoor) %>%
  print()

male_abpoor_mode <- Mode(male_data$abpoor) %>%
  print()

# Summary statistics for 'madeg'
female_madeg_mid <- summary(female_data$madeg) %>%
  print()

male_madeg_mid <- summary(male_data$madeg) %>%
  print()