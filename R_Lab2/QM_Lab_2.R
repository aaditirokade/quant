#########################################################
#####  R Lab 2: Charts and Graphs -- 2006 GSS Data  #####
#########################################################
install.packages("tidyverse")
library(tidyverse)


# Importing GSS 2006 Data
GSS2006 <- read_csv("/Users/aaditirokade/Desktop/Quant/R_Lab_2/GSS_2006.csv")

help("count")                 #opens in the help tab
help("unique")                #Extract Unique Elements
help("mutate")
example("count")              #Gives an example

# Case summary for "polviews'

# We can use unique() to see what are all of the distinct 
#observations of the variable we are interested in.
unique(GSS2006$polviews) 

  count(GSS2006, polviews) %>%              
  filter(!is.na(polviews)) %>% 
  arrange(desc(n)) %>%  # Arrange in descending order
  mutate(percent = (n/sum(n)*100))

# Pie chart 'race'

# We can first get a glimpse of out data
count(GSS2006, race)           #gives a count for each race

# We then use the numebers from the output
# above to create a simple pie chart
slices_06 <- c(634, 592, 3284)                # inputing the numbers for a slice of the pie chart
lbls_06 <- c("Black", "Other", "White")       # labelng the slices of the pie chart correctly
pct_06 <- round(slices_06/sum(slices_06)*100) # rounding the numbers and multiplying by 100
lbls_06 <- paste(lbls_06, pct_06) # add percents to labels 
lbls_06 <- paste(lbls_06,"%",sep="") # add % to labels 
pie(slices_06,labels = lbls_06, col=rainbow(length(lbls_06)), 
main="Percent Distribution of Racial Groups - 2006")


# Bar Chart
count(GSS2006, conclerg)          #how did people respond

GSS2006 %>%                       #plots a chart or mapping
  filter(!is.na(conclerg)) %>%  # removing all NA's
  ggplot(mapping = aes(x = conclerg, y=..count.., fill = conclerg)) +
  geom_bar()+ 
  geom_text(stat = "count", aes(label=..count..), vjust=1.5) + # adding the raw totals to the bars
  ggtitle("Confidence in Clergy - GSS 2006")+ # main title
  labs(x = "Confidence in Organized Religion")  # axis labels


GSS2006 %>%                    #similar but 2 different instructions for plot
  filter(!is.na(conclerg)) %>% # removing all NA's
  ggplot(mapping = aes(x = conclerg, y=(..count..)/sum(..count..), fill = conclerg)) +
  geom_bar()+
  geom_text(stat = "count", 
            aes(label = scales::percent((..count..)/sum(..count..))), 
            vjust=1.5) + # adding the percentage numbers to the bars
  ggtitle("Percent Distribution of Confidence in Clergy - 2006")+ # main title
  labs(x = "Confidence in Organized Religion", y = "Percent") # axis labels
  


# Histogram for 'childs'
unique(GSS2006$childs) # unique observations

# have to recode all observatiosn as `eight or more' is problematic
# if trying to plot a histogram
GSS2006$childs <- recode(GSS2006$childs,
                       "0" = 0,
                       "1" = 1,
                       "2" = 2,
                       "3" = 3, 
                       "4" = 4,
                       "5" = 5,
                       "6" = 6,
                       "7" = 7,
                       "eight or more" = 8)

unique(GSS2006$childs) # recheck the unique values to
summary(GSS2006$childs) # summary statistics of `childs'

# plotting histogram using ggplot2
ggplot(GSS2006, aes(x=childs)) +
  geom_histogram(binwidth=1.1, colour="black", fill="lightblue") +
  geom_vline(aes(xintercept=mean(childs, na.rm=T)), # adding line for mean
             color="red", linetype="dashed", size=1) + # Ignore NA 
  ggtitle("Number of Children in Household - 2006") + # main title
  labs(x = "Number of Children", y = "Frequency") # axis labels
  

