
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