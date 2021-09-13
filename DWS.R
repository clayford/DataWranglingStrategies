# Data Wrangling Strategies with R
# Fall 2021
# UVA Library - Research Data Services
# Clay Ford


# load packages
library(tidyverse)
library(lubridate)


# Download Data for Workshop ----------------------------------------------

# The data we'll use in this workshop is available in a 5 MB ZIP file on a
# GitHub repository. The following code will download and unzip the file. Before
# running the following code you may want to set your working directory to your
# Desktop or a new folder with a name like "DWS_workshop".

# To set working directory: 
# Session...Set Working Directory...Choose Directory...

URL <- "https://github.com/clayford/DataWranglingStrategies/raw/main/DWS_data.zip"
d <- basename(URL)
download.file(url = URL, destfile = d)
unzip(d)
setwd("DWS_data")
rm(URL, d)



# Data Structures ---------------------------------------------------------


# Vector: 1D object of same data type (eg, all numeric, or all character). Like
# a column of data in a spreadsheet. We can create a vector with c()
x <- c(1, 4, 23, 4, 45)

# Data frame: 2D object consisting of vectors of the SAME LENGTH but
# of potentially different data types
n <- c(1, 3, 5)
g <- c("M", "M", "F")
df <- data.frame(n, g)
df
str(df)


# Lists are a general data structure that can hold data of any shape or type. We
# can create a list with the list() function.
lst <- list(x, n, g, df)
lst

# Use double brackets to extract list elements
lst[[4]]

# Using single brackets return the element in a list
lst[4]

# We can also name the elements of a list
lst2 <- list(num = x, size = n, sex = g, data = df)
lst2

# Access the list elements using the $ operator and their name
lst2$num
lst2$data

# Many statistical results are stored as a list. For example, a t-test.
ttest <- t.test(1:10, y = 7:20)
ttest$p.value
ttest$statistic
ttest$estimate


# A data frame is a list with vectors of equal length.
df$n
df$g
is.list(df)

# Applying functions to lists ---------------------------------------------

# A toy example list for demonstration:
lst3 <- list(x = c(1, 3, 5, 7),
             y = c(2, 2, 2, 4, 5, 5, 5, 6),
             z = c(22, 3, 3, 3, 5, 10))
lst3

# find the means of x, y, z.

# can do one at a time
mean(lst3$x)
mean(lst3$y)
mean(lst3$z)

# The lapply() function allows us to "apply" a function to each list element
lapply(lst3, mean)

# sapply() does the same but simplifies the output
sapply(lst3, mean)

# Since a data frame is list, we can apply functions to it as well. For example,
# apply the typeof function to each column. typeof tells us the type of object.
lapply(df, typeof)
sapply(df, typeof)

# CODE ALONG 1 ------------------------------------------------------------

# lapply and sapply the summary and quantile functions to lst3. How does sapply
# simplify the result?




# Binding -----------------------------------------------------------------

# Let's create some fake data to demonstrate binding. 

dat01 <- data.frame(x = 1:5, y = 5:1)
dat01
dat02 <- data.frame(x = 10:16, y = 10:16/2)
dat02
dat03 <- data.frame(z = runif(5)) # 5 random numbers from interval (0,1)
dat03

# row binding
# ie, stack data frames

# dplyr's bind_rows() works on data frames or a list of data frames. Columns are
# matched by name, and any missing columns will be filled with NA.

# stack dat01 and dat02
bind_rows(dat01, dat02)

# save the new stacked data frame
dat04 <- bind_rows(dat01, dat02)
dat04

# we can use the same data frames multiple times
bind_rows(dat01, dat02, dat01)

# Example of binding data frames with no matching columns.
bind_rows(dat01, dat03)

# We can use the optional ".id" argument to create a new column that contains an
# identifier for the original data.
bind_rows(dat01, dat02, .id = "id")

# This might be useful if you were row binding multiple data sets for, say,
# different classrooms and you wanted a classroom-level identifier.

# Naming the data frames that we're binding provides a useful label in the id 
# column.
bind_rows("dat01" = dat01, "dat02" = dat02, .id = "id")

# bind_rows() also works on lists of data frames
list01 <- list("dat01" = dat01, "dat02" = dat02)
list01
bind_rows(list01)
bind_rows(list01, .id = "source")


# column binding
# ie, set data frames side-by-side

# dplyr's bind_cols() works on data frames or a list of data frames. Rows are
# matched by position, so all data frames must have the same number of rows.

# place dat01 and dat03 side-by-side
bind_cols(dat01, dat03)

# The following throws an error since the data frames do not share the same
# number of rows.
bind_cols(dat01, dat02)


# Extended example: reading multiple data files ---------------------------


setwd("stocks")
# get file names
stocks <- list.files()  
# apply read_csv to each file name; return a list
stocks_ls <- lapply(stocks, read_csv)  

# lapply(stocks, read_csv) essentially does the following:

# stocks_ls <- list(read_csv("bbby.csv"), 
#      read_csv("flws.csv"), 
#      read_csv("foxa.csv"), 
#      read_csv("ftd.csv"), 
#      read_csv("tfm.csv"), 
#      read_csv("twx.csv"), 
#      read_csv("viab.csv"))

# stocks_ls is a list of 7 data frames
length(stocks_ls)

# The seven list elements have no name
names(stocks_ls)

# Before row binding, let's name each list element so we can use the .id
# argument to identify the stock in the final data frame.

# name each list element (replace ".csv" with nothing);
# str_remove() is a stringr function that removes the specified text pattern.
names(stocks_ls) <- str_remove(stocks, ".csv")
names(stocks_ls)

# Use bind_rows to combine all data frames in list to one data frame. Because
# each list element has a name and we use the .id argument, a new column will be
# created that indicates the stock.
stocks_df <- bind_rows(stocks_ls, .id = "stock")

# Let's calculate change from opening to closing price each day
stocks_df$Change <- stocks_df$Open - stocks_df$Close

# first six records
head(stocks_df)


# CODE ALONG 2 ------------------------------------------------------------

# VA DOE: Public high school graduate and completer data for 2006 - 2016.
# http://www.doe.virginia.gov/statistics_reports/research_data/index.shtml

# Read in all CSV files and bind into one data frame called "grads_df".

# TIP: look at one of the CSV files before starting. Do you need to use the .id
# argument?
setwd("../doe")

filenames <- list.files()
grads_lst <- lapply(filenames, read_csv)
grads_df <- bind_rows(grads_lst)


# Working with dates and times --------------------------------------------

# We often want to calculate elapsed number of days or seconds. Or extract month
# or day of week from a date, so we can compare quantities by month or day.

# R allows us to store dates as number of days since Jan 1, 1970, and times as
# number of seconds since Jan 1, 1970. The Jan 1, 1970 date is arbitrary. It
# just allows us to convert a date into a number that we can use for analysis.

# The lubridate package provides functions for working with dates. 

# It provides a series of functions for parsing dates that are a permutation of
# the letters m, d and y to represent the ordering of month, day and year.

# Examples:
date1 <- mdy("January 1, 2013")
as.numeric(date1) # days since 1/1/1970
date2 <- ymd("1776-07-04")
as.numeric(date2)

# Use wday, month, and year functions to extract components of date
wday(date2, label = TRUE)
month(date2, label = TRUE)
year(date2)

# Let's format the dates in stocks_df. The date is in the format day-month-year,
# so we use the dmy function.
head(stocks_df$Date)
stocks_df$Date <- dmy(stocks_df$Date)

# Dates print like character values but are actually numbers.
head(stocks_df$Date)
head(as.numeric(stocks_df$Date))

# Having the Date properly formatted allows us to extract Day and Month with
# ease.

# Extract day of week and save into new column called "Day"; label = TRUE uses
# name of day instead of the number.
stocks_df$Day <- wday(stocks_df$Date, label = TRUE)

# Extract month of year and save into new column called "Month"; label = TRUE
# uses name of month instead of the number.
stocks_df$Month <- month(stocks_df$Date, label = TRUE)


# first six records
head(stocks_df)

# Formatting the date and extract day of week allows to easy create plots
# involving time.

# plot closing price over time for all stocks
ggplot(stocks_df, aes(x = Date, y = Close, color = stock)) +
  geom_line() 

# boxplots of change by day of week for each stock
ggplot(stocks_df, aes(x = Day, y = Change)) +
  geom_boxplot() +
  facet_wrap(~stock)



# CODE ALONG 3 ------------------------------------------------------------

# NYPD Shooting Incident Data 

# List of every shooting incident that occurred in NYC going back to 2006
# through the end of the previous calendar year.
# https://catalog.data.gov/dataset/nypd-shooting-incident-data-historic

# import the file "NYPD_Shooting_Incident_Data__Historic_.csv", 
# format the OCCUR_DATE column as a date,
# create a column called OCCUR_DAY that lists day of week.
# create a column called OCCUR_YEAR that lists year.
# What day of the weeks do most shootings happen?
# How are numbers of shooting changing over time?

setwd("..")
sid <- read.csv("NYPD_Shooting_Incident_Data__Historic_.csv") 
sid$OCCUR_DATE <- mdy(sid$OCCUR_DATE)
sid$OCCUR_DAY <- wday(sid$OCCUR_DATE, label = TRUE)
sid$OCCUR_YEAR <- year(sid$OCCUR_DATE)

barplot(table(sid$OCCUR_DAY))
barplot(table(sid$OCCUR_YEAR))

# Merging/Joining ---------------------------------------------------------

# Let's create some fake data to demonstrate merging/joining data:
left <- data.frame(id=1:3,
               x=c(12, 14, 16))
right <- data.frame(id=2:4,
                y=c("a", "b", "c"))


#### left join

# If we want to retain everything in the left data frame and merge only what 
# has a matching id in the right data frame, we do a LEFT JOIN.
left; right
left_join(left, right, by = "id")

# Notice all rows from left are retained and NAs are created in the grp column
# where the right had no matching id. This is why it's called a "left join".

#### right join

# If we want to retain everything in the right data frame and merge only what 
# has a matching id in the left data frame, we do a RIGHT JOIN.
left; right
right_join(left, right, by = "id")

# Notice all rows from right are retained and NAs are created in the grp column
# where the left had no matching id. This is why it's called a "right join".

#### inner join

# If we want to retain only those rows with matching ids in BOTH data sets, we
# do an INNER JOIN.
left; right
inner_join(left, right, by = "id")

# Notice only those records with matching ids are joined.

#### full join

# If we wanted to merge ALL rows regardless of match, we do a FULL JOIN.
left; right
full_join(left, right, by = "id")

# Notice all rows from both data frames are retained and NAs are created in 
# columns where rows did not have matching ids in the other data set. 


#### merging with multiple keys

# Sometimes we have more than one key in each data frame that we want to merge
# on. In that case we give the by argument a vector of keys
left2 <- data.frame(id1 = c(1,1,2,2), 
                id2 = c(1,2,1,2), 
                x = c(101, 103, 105, 107))

right2 <- data.frame(id1 = c(1,1,2), 
                id2 = c(1,2,3), 
                y = c("a","b","c"))


# left join
left2; right2
left_join(left2, right2, by = c("id1", "id2"))



#### merging with multiple keys and keys with different names

# The examples above were clean: The keys had the same names. It's rarely that
# simple in real life. Below is an example of how to merge data using multiple
# keys with different names. Same data as above but with different names for the
# keys.

left2 <- data.frame(id1 = c(1,1,2,2), 
                id2 = c(1,2,1,2), 
                x = c(101, 103, 105, 107))

right2 <- data.frame(V1 = c(1,1,2), 
                 V2 = c(1,2,3), 
                 y = c("a","b","c"))

# Let's say columns "id1" and "id2" in left2 correspond to columns "V1" and "V2"
# in right2. To perform a left join with these data frames we use a named
# vector.

left2; right2
left_join(left2, right2, by = c("id1" = "V1", "id2" = "V2"))

# The same modifications will work for right joins, inner joins, and full joins.



# The dplyr package also provides functions for performing "filtering joins"
# which is a way to check if rows in one data frame have (or do not have)
# membership in another data frame.

# Once again let's create some fake data to demonstrate. 
ages <- data.frame(id = 1:5, 
               name = c("Rick", "Morty", "Jerry", "Beth", "Summer"),
               age = c(67, 15, 42, 39, 17))
grp <- data.frame(ID = 1:3,
              GRP = c(1, 1, 2))

#### semi join

# I only want to keep rows in ages that have a match in grp
ages; grp
semi_join(ages, grp, by = c("id" = "ID"))

#### anti join

# I only want to keep rows in ages that do NOT have a match in grp
ages; grp
anti_join(ages, grp, by = c("id" = "ID"))



# Extended example: merging stock data and Google trends data -------------

# read in historical stock data for Apple (obtained from Yahoo Finance)
aapl <- read_csv("AAPL.csv")
summary(aapl)
head(aapl$Date)

# format date using mdy() function from the lubridate package
aapl$Date <- mdy(aapl$Date)
summary(aapl)


# read in Google trends data for "new macbook pro 2018". Notice we skip the
# first three lines and specify the column names.
gt <- read_csv("mac_book_pro_trends.csv",
               skip = 3, 
               col_names = c("Date","Interest"))
summary(gt)

# Let's merge the stock data with the Google Trends data. It appears we can
# merge on Date.
names(aapl)
names(gt)

inner_join(aapl, gt, by = "Date")

# All stock data is Monday - Friday while Google trends data is weekly on
# Sunday. No records in common by Date so inner_join returns an empty data
# frame.

table(wday(aapl$Date, label = TRUE))
table(wday(gt$Date, label = TRUE))

# Simple hack: change Google trends date to Monday by adding 1
gt$Date <- gt$Date + 1
table(wday(gt$Date, label = TRUE))

# Perform inner join to merge records from both data frames with matching dates,
# and save as a new data frame.
aapl_gt <- inner_join(aapl, gt, by = "Date")
aapl_gt

# Is there any association between Google trends and closing price? Plot Closing
# Price vs Interest and add a smooth trend line
ggplot(aapl_gt, aes(x = Interest, y = Close)) + 
  geom_point() +
  geom_smooth(se = F)



# Regular expressions -----------------------------------------------------

# Regular expressions are a language for defining text patterns. 

# Example: remove the suffix from the vector of names
names <- c("Ford, MS", "Jones, PhD", "Martin, Phd", "Huck, MA, MLS")

# pattern: first comma and everything after it
# Here are two possible ways.
str_remove(names, pattern = ", [a-zA-Z, ]+")
str_remove(names, pattern = ", [[:print:]]+")

# [a-zA-Z, ]+ = one or more of the letters Aa - Zz, comma, and space
# [[:print:]]+ = one or more printable characters

# See the slide deck for this workshop for a slightly deeper dive.

# The R for Data Science book has a good overview of regular expressions:
# https://r4ds.had.co.nz/strings.html

# The suggested strategy for using regular expressions: know when you need them
# and use Google and trial-and-error to create the right one.


# CODE ALONG 4 ------------------------------------------------------------

# Let's merge school population totals into our VA DOE data so we can calculate proportion of disadvantaged youth who completed high school.

# (1) Read in school population totals for all Virginia schools for the
# 2016-2017 year: va_schools_2016-2017.csv

# (2) Remove non-alpha-numeric characters from column names using regular
# expressions.

# (3) Add leading 0s to DivNo and SchoolNo using the str_pad() function.
# DinNo should be width 3 (eg "001")
# SchoolNo should be width 4 (eg, "0001")

# (4) Subset grads_df to only include rows where SCHOOL_YEAR == "2016-2017" and
# name the new data frame "grads_df_2016_2017"

# (5) Merge the grads_df_2016_2017 and va_schools data frames based on Division
# and School Number such that all rows from grads_df_2016_2017 are retained.
# Save the new data frame as va2016_2017

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

# When finished we can find schools with the highest rate of economically
# disadvantaged completers who obtained a Standard Diploma.

# First calculate percentage of completers; divide the completer count by number
# of students in Grade 12
va2016_2017$pctComplete <- va2016_2017$HS_COMPLETER_CNT/va2016_2017$Grade12

# The data dictionary tells us that a missing (or Null) value in a field means
# it was not considered when compiling the count for the record.

# Data dictionary:
# http://www.doe.virginia.gov/statistics_reports/research_data/data_files/data_dictionary.pdf

# The tidyverse allows us to use the pipe %>% to send the output of one function
# into the first argument of the 2nd function.

va2016_2017 %>% 
  filter(DISADVANTAGED_FLAG == "Y" & 
           is.na(GENDER) & is.na(FEDERAL_RACE_CODE) & 
           is.na(DISABILITY_FLAG) & is.na(LEP_FLAG) & 
           HS_COMPLETION_NAME == "Standard Diploma" &
           LEVEL_CODE == "SCH") %>% 
  select(SCH_NAME, DIV_NAME, HS_COMPLETER_CNT, Grade12, pctComplete) %>% 
  arrange(desc(pctComplete))



# Reshaping ---------------------------------------------------------------

# It's often helpful to think of data as "wide" or "long". 

# Example of a wide data frame. Notice each person has multiple test scores
# that span columns.
wide <- data.frame(name=c("Clay","Garrett","Addison"), 
                   test1=c(78, 93, 90), 
                   test2=c(87, 91, 97),
                   test3=c(88, 99, 91))
wide

# Example of a long data frame. This is the same data as above, but in long
# format. We have one row per person per test.
long <- data.frame(name=rep(c("Clay","Garrett","Addison"),each=3),
                   test=rep(1:3, 3),
                   score=c(78, 87, 88, 93, 91, 99, 90, 97, 91))
long

# The long format is actually preferable for many scenarios in R. Hadley Wickham
# coined a term called "tidy data" to describe it. In tidy data, each variable 
# is a column and each observation is a row. Here we have 3 variables: name,
# test, and score. Each row represents a single observation on a student. 

# With data in this format we can easily summarize and plot the data. For example:

# mean score per student
aggregate(score ~ name, data = long, mean)
aggregate(score ~ test, data = long, mean)

# line plot of scores over test, grouped by name
ggplot(long, aes(x = factor(test), y = score, color = name, group = name)) +
  geom_point() +
  geom_line() +
  xlab("Test")


# R for Data Science has a chapter called Tidy Data that goes into further detail:
# http://r4ds.had.co.nz/tidy-data.html


#### reshape wide to long

# The tidyr package provides functions for reshaping data. To reshape wide data
# into long format we use the pivot_longer() function.

wide
pivot_longer(wide, test1:test3, names_to = "test", values_to = "score")

# The first argument is the dataset to reshape
 
# The second argument describes which columns need to be reshaped.

# The names_to argument gives the name of the variable that will be created from
# the data stored in the column names, i.e. test
 
# The values_to argument gives the name of the variable that will be created
# from the data stored in the cell value, i.e. score


# Other ways to accomplish the same thing:

# specify all columns except "name" using a minus sign
pivot_longer(wide, -name, names_to = "test", values_to = "score")

# drop "test" from the test column with names_prefix argument
pivot_longer(wide, -name, names_to = "test", values_to = "score", 
             names_prefix = "test")

#### reshape long to wide 

# This is less common. For this we use the tidyr function pivot_wider().

long
pivot_wider(long, name, names_from = test, values_from = score)

# The first argument is the dataset to reshape

# The second argument describes which columns need to be reshaped.

# The names_from argument gives the name of the variable that contains the data
# that will be used to create the column names.

# The values_from argument gives the name of the variable that contains the
# data that will be used to populate the cells.

# using the names_prefix argument lets us prepend text to the column names.
pivot_wider(long, name, names_from = test, values_from = score,
            names_prefix = "test")


# For more on pivot_longer() and pivot_wider(), see the package vignette
vignette("pivot", package = "tidyr")



# Extended example: reshape stocks_df to be "long" ------------------------

# If we examine the column headers of stocks_df we can see that we really have
# five variables instead of seven: (1) Stock, (2) Date, (3) price type, (4)
# price, and (5) volume. Below we use pivot_longer() to make the data "long".

head(stocks_df)
stocks_df_long <- pivot_longer(stocks_df, Open:Close, names_to = "price_type", values_to =  "price")
head(stocks_df_long)

# With our data in "long" format it's a little easier to create plots that show
# both high and low prices.
ggplot(filter(stocks_df_long, price_type %in% c("High","Low")), 
       aes(x = Date, y = price, color = price_type)) +
  geom_line() +
  facet_wrap(~stock, scales = "free") 


# CODE ALONG 5 ------------------------------------------------------------


# The file "mhp.csv" contains data on the number of patients in mental hospitals
# (in thousands), by type of hospital, from 1904-1970. Each year's data was
# estimated in July.

# (1) Read in the data.

# (2) Reshape mhp to have three columns: Year, HospitalType, and NumberPatients.
# Save the new data frame as "mhp_long". The first few rows of mhp_long should
# look like this:

#   Year  HospitalType  NumberPatients
# 1  1923 Federal                  29
# 2  1923 State                   230
# 3  1923 Private                   9

mhp <- read.csv("mhp.csv")
head(mhp)

mhp_long <- pivot_longer(mhp, Federal:Private, 
                         names_to = "HospitalType", 
                         values_to = "NumberPatients")
head(mhp_long)

# If reshaped correctly, the following code should produce a plot with
# State hospitals showing a steady increase through about 1955 and then a steep
# decline, while Federal and Private numbers remained flat. 
ggplot(mhp_long, aes(x=Year, y=NumberPatients, 
                     color=HospitalType, group=HospitalType)) + 
  geom_line() + 
  labs(y="Number of Patients (Thousands)")



# About the data ----------------------------------------------------------

# Stock data from Yahoo and Google Finance:
# https://finance.yahoo.com/
# https://www.google.com/finance

# Google Trends: https://trends.google.com/trends/?geo=US

# Virginia Department of Education:
# http://www.doe.virginia.gov/statistics_reports/research_data/index.shtml

# NYPD Shooting Incident Data (Historic) 
# https://catalog.data.gov/dataset/nypd-shooting-incident-data-historic

# Steckel, Richard H. , "Patients in mental hospitals, by type of hospital:
# 1904-1970 ." Table Bd212-216 in Historical Statistics of the United States,
# Earliest Times to the Present: Millennial Edition, edited by Susan B. Carter,
# Scott Sigmund Gartner, Michael R. Haines, Alan L. Olmstead, Richard Sutch, and
# Gavin Wright. New York: Cambridge University Press, 2006.
# http://dx.doi.org/10.1017/ISBN-9780511132971.Bd63-24010.1017/ISBN-9780511132971.Bd63-240



# Base R and the tidyverse ------------------------------------------------

# Due to R's power, flexibility, and maturity, there are numerous ways to 
# accomplish data wrangling tasks in R. One way is to use "Base R" functions, 
# which are the functions that come with R. No need to install extra packages. 
# Just open R (or RStudio) and they're ready to go. If you learned R prior to
# about 2012, this is most likely how you learned to work with data.

# Another increasingly popular way to work with data in R is using what are 
# called the "tidyverse" packages. I'll quote their web site: "The tidyverse is 
# a collection of R packages that share common philosophies and are designed to 
# work together." The tidyverse packages provide an alternative to the "Base R
# way" of working with data, particularly (but not exclusively) data frames.

# You can also learn more at http://tidyverse.org/ and by reading R for Data
# Science at http://r4ds.had.co.nz/


# The verbs of data manipulation ------------------------------------------

# Data manipulation, or data wrangling, usually involves performing the same
# routine actions on a data set you've imported. You can think of the following
# as data wrangling verbs, but they are also dplyr functions:

# - select: selecting (or not selecting) columns based on their names 
#           (eg: select columns Q1 through Q25)

# - slice: selecting (or not selecting) rows based on their position 
#           (eg: select rows 1:10)

# - mutate: add or derive new columns (or variables) based on existing columns 
#           (eg: create a new column that expresses measurement in cm based on 
#           existing measure in inches)

# - rename: rename variables or change column names 
#           (eg: change "GraduationRate100" to "grad100")

# - filter: selecting rows based on a condition 
#           (eg: all rows where gender = Male)

# - arrange: ordering rows based on variable(s) numeric or alphabetical order
#            (eg: sort in descending order of Income)

# - sample: take random samples of data 
#           (eg: sample 80% of data to create a "training" set)

# Two other functions of note:

# - summarize: condense or aggregate multiple values into single summary values
#              (eg: calculate median income by age group)

# - group_by: convert a tbl into a grouped tbl so that operations are performed 
#            "by group"; allows us to summarize data or apply verbs to data by 
#            groups (eg, by gender or treatment)


# Last but not least, the pipe: %>% 
# Use Ctrl + Shift + M (Win) or Cmd + Shift + M (Mac) to enter in RStudio

# The pipe takes the output of a function and "pipes" into the first argument of
# the next function.

# Example: Instead of this...
round(mean(stocks_df$Open))
# with pipes we can do this...
mean(stocks_df$Open) %>% round()

# Read a pipe as "then"

# Some dplyr examples:

# (1) Take the stocks data frame, then group by stock, and then calculate the
# mean closing price:

stocks_df %>% 
  group_by(stock) %>% 
  summarize(mean_open = mean(Open))

# With summarize we can create many summaries
stocks_df %>% 
  group_by(stock) %>% 
  summarize(mean_open = mean(Open),
            mean_close = mean(Close),
            median_open = median(Open),
            median_close = median(Close))

# (2) Take the stocks data frame, then sort by closing price in descending
# order, then only show date, stock and closing price columns, and then show the
# top 10 results
stocks_df %>%
  arrange(desc(Close)) %>% 
  select(Date, stock, Close) %>% 
  top_n(n = 10)

# (3) Take the stocks data frame, then group by stock, then sort by closing
# price in descending order, then only show date, stock and closing price
# columns, and then show the top 10 results for EACH STOCK
stocks_df %>%
  group_by(stock) %>% 
  arrange(desc(Close)) %>% 
  select(Date, stock, Close) %>% 
  top_n(n = 10)

# The above works, but only shows the first few rows. That's how tibbles print.
# Append as.data.frame() to see all rows in the console
stocks_df %>%
  group_by(stock) %>% 
  arrange(desc(Close)) %>% 
  select(Date, stock, Close) %>% 
  top_n(n = 10) %>% 
  as.data.frame()

# (4) take the stocks data frame, then rename stock to Stock, Open to Opening,
# and Close to Closing. Notice we have to reassign the result to make the
# changes permanent.
stocks_df <- stocks_df %>% 
  rename(Stock = stock,
         Opening = Open,
         Closing = Close)    # New name = Old name


# (5) take the stocks data frame, then subset for Fridays, then create an
# indicator that is 1 when change is greater than 0 and 0 otherwise, then group
# by stock, and then find the proportion of times a stock closed higher than it
# opened on Fridays.

stocks_df %>% 
  filter(Day == "Fri") %>% 
  mutate(Higher = if_else(Change > 0, 1, 0)) %>% 
  group_by(Stock) %>% 
  summarise(mean_higher = mean(Higher))


# The free R for Data Science book at http://r4ds.had.co.nz/ has much more!