#####-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#####
#####   using tidyr, dplyr, and the pipe operator   #####
#####   University of Guelph R users group (UGRU)   #####
#####   Last updated: 04 October 2018               #####
#####-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#####

## set working directory
getwd()

## install and call required packages
pkgs <- c("tidyverse", "truncnorm", "lubridate", "ggthemes", "ggExtra")
# lapply(pkgs, install.packages, character.only = TRUE)
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

## set plotting theme
theme_set(theme_few())


# create stock price dataset ----------------------------------------------

stocks_main <- tibble(date = as.Date("2009-01-01") + 0:99, 
                      X = rtruncnorm(n = 100, a = 0, b = Inf, mean = 2.4, sd = 0.5), 
                      Y = rtruncnorm(100, 0, Inf, 1.2, 0.4), 
                      Z = rtruncnorm(100, 0, Inf, 0.8, 0.2))

## COMMENT: 
## we have just generated a dataset containing the average daily prices of 
## three stocks over a 100 day period. 
## Each stock is given its own column -- this is called 'wide-format'.
## This might be an intuitive way to collect the data, but what happens when we want
## to plot it or run a model? 'Long-format' data is usually (always?) more 
## convenient for these operations.


# using tidyr::gather() to reshape data -----------------------------------

?gather()

## convert stocks from wide- to long-format
stocks <- gather(data = stocks_main, key = stock, value = price, ... = 2:4)

## plot the value of each stock through time
ggplot(data = stocks,
       mapping = aes(x = date, y = price, colour = stock)) + 
  geom_line()


# using tidyr::separate() to split a column -------------------------------

?separate()

## generate three new columns (day, month, year) from the date column
(stocks <- separate(data = stocks, col = date, 
                   into = c("year", "month", "day"), 
                   sep = "-", remove = FALSE))

## COMMENT:
## the three new columns are all given the default class 'character', whereas
## the original date column is of class 'date'. Why might this matter? 
## How can we correct this? [see using dplyr::mutate() below]

## remove the year and day columns from the dataset
(stocks <- select(stocks, -c("year", "day")))

## make plots of the stock prices per month
ggplot(data = stocks,
       mapping = aes(x = month, y = price, fill = stock)) + 
  geom_violin() + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "pointrange", position = position_dodge(0.9))


# using dplyr::mutate() to create/modify a variable -----------------------

?mutate()

## modify the month column so it is recognized as a date object
stocks <- mutate(.data = stocks, month = lubridate::month(date, label = TRUE))

## create a new column with the four-letter tickers for each stock
stocks <- mutate(.data = stocks,
                 ticker = as.factor(case_when(
                   stock == "X" ~ "ABCD", 
                   stock == "Y" ~ "MNOP", 
                   stock == "Z" ~ "STUV")))

## COMMENT:
## the case_when() function rocks!!


# using dplyr::filter() to subset your data -------------------------------

## what are the unique months exist in our dataset?
unique(stocks$month)

## subset the data to include stock prices for February and March only
stocks_subFM <- filter(.data = stocks, month %in% c("Feb", "Mar"))

## COMMENT:
## the convenient %in% operator allows us to pass a list of elements to 
## an argument; it's also commonly used for value matching (e.g., 3:5 %in% 1:10)


# using dplyr::summarise() to calculate summary statistics ----------------

?summarise()

## calculate the monthly mean and standard deviation for each stock
(stock_summ <- summarise(.data = stocks_subFM %>% 
                           group_by(ticker, month), 
                         mean = mean(price), 
                         stdev = sd(price)))

## COMMENT:
## notice the use of the group_by() function in combination with the pipe %>%, 
## which allowed us to first split the dataset by stock and by month, and 
## then calculate our summary statistics for each stock


# using the pipe to write concise and readable code -----------------------

## rm objects from workspace
rm(stocks, stock_subFM, stocks_summ)

## modify the original dataset
stocks <- stocks_main %>% 
  gather(key = stock, value = price, ... = 2:4) %>% 
  separate(col = date, into = c("year", "month", "day"), 
           sep = "-", remove = FALSE) %>% 
  select(-c("year", "day")) %>% 
  mutate(month = month(date, label = TRUE), 
         ticker = as.factor(case_when(
           stock == "X" ~ "ABCD", 
           stock == "Y" ~ "MNOP", 
           stock == "Z" ~ "STUV")))

## calculate summary values
(stocks_summ <- stocks %>% 
  group_by(ticker, month) %>% 
  summarise(min = min(price), 
            max = max(price), 
            mean = mean(price), 
            stdev = sd(price)))

## plot the change in stock prices through time
plot <- ggplot(data = stocks, 
               mapping = aes(x = date, y = price, colour = stock)) + 
  geom_point() + 
  geom_line() + 
  labs(x = "Date", y = "Stock price ($)", 
       title = "Daily average stock prices between \n01-Jan-2009 and 10-Apr-2009")
ggExtra::ggMarginal(p = plot, type = "boxplot", margins = "y", groupFill = TRUE)

## -- ## -- ## -- ## -- ## END OF SCRIPT ## -- ## -- ## -- ## -- ##