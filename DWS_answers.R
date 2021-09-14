# Data Wrangling Strategies code along answers

# CODE ALONG 2 ------------------------------------------------------------

filenames <- list.files()
grads_lst <- lapply(filenames, read_csv)
grads_df <- bind_rows(grads_lst)


# CODE ALONG 3 ------------------------------------------------------------

sid$OCCUR_DATE <- mdy(sid$OCCUR_DATE)
sid$OCCUR_DAY <- wday(sid$OCCUR_DATE, label = TRUE)
sid$OCCUR_YEAR <- year(sid$OCCUR_DATE)

barplot(table(sid$OCCUR_DAY))
barplot(table(sid$OCCUR_YEAR))



# CODE ALONG 4 ------------------------------------------------------------


va_schools  <- read_csv("va_schools_2016-2017.csv")
names(va_schools)

names(va_schools) <- str_remove_all(names(va_schools), 
                                    pattern = "[[:punct:][:space:]]")


head(va_schools$DivNo)
head(va_schools$SchoolNo)

va_schools$DivNo <- str_pad(va_schools$DivNo, width = 3, 
                            side = "left", pad = "0")
va_schools$SchoolNo <- str_pad(va_schools$SchoolNo, width = 4, 
                               side = "left", pad = "0")

grads_df_2016_2017 <- subset(grads_df, SCHOOL_YEAR == "2016-2017")


names(grads_df_2016_2017)[c(3,5)]
names(va_schools)[c(1,3)]

va2016_2017 <- left_join(grads_df_2016_2017, va_schools, 
                         by = c("DIV_NUM" = "DivNo", "SCH_NUM" = "SchoolNo"))



# CODE ALONG 5 ------------------------------------------------------------

mhp <- read.csv("mhp.csv")
head(mhp)

mhp_long <- pivot_longer(mhp, Federal:Private, 
                         names_to = "HospitalType", 
                         values_to = "NumberPatients")
head(mhp_long)
