#####  R Lab 5: Cross-tabulation Tables and Chi-Square Analysis -- 2016 GSS Data  ####

install.packages("tidyverse")
install.packages("oii")
library(tidyverse)

#Oxforsd Internet Institute(for cross tabulation tables)
library(oii)


# Import GSS 2016 Data
GSS2016 <- read_csv("GSS_2016.csv")


# Create a new data frame selecting only "class" and "gunlaw":
gun_cls_2016 <- GSS2016 %>% 
  select(class, gunlaw) %>% 
  filter(!is.na(class)) %>%
  filter(!is.na(gunlaw))

# we're going to create a table from
table_1 <- table(gun_cls_2016$gunlaw, gun_cls_2016$class) %>% 
  print()

# Generating the cross-tab and chi-square analysis
oii.xtab(table_1, col = TRUE, row = TRUE,stats = TRUE, 
         varnames = c("Class", "View on Gun Laws"))

