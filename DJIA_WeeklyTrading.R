# Title     : DJIA Weekly Trading
# Objective : To find the DJIA stock which will deliver most profit next week.
# Created by: Partha S Mukherjee
# Created on: 5/24/2020

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")

tickers <- c("MMM", "AXP", "AAPL", "BA", "CAT", "CVX", "CSCO", "KO", "DOW", "XOM", "GS", "HD", "IBM", "INTC", "JNJ",
           "JPM", "MCD", "MRK", "MSFT", "NKE", "PFE", "PG", "RTX", "TRV", "UNH", "VZ", "V", "WMT", "WBA", "DIS")
################################ Data Wrangling #####################################
# Download the data into data directory as ticker.csv file
if(!dir.exists("data")) dir.create("data")

url_tmpl <- "https://query1.finance.yahoo.com/v7/finance/download/%s?period1=%s&period2=1578009600&interval=1wk&events=history"
data_map <- tibble(
start_period = c(915408000, 915408000, 1427068800, 915408000, 915408000, 915408000, 1244419200, 915408000, 915408000,
                 915408000, 1379894400, 941414400, 941414400, 941414400, 941414400, 941414400, 941414400, 941414400,
                 941414400, 1379894400, 1081382400, 915408000, 915408000, 1244419200, 1348444800, 1081382400,
                 1379894400, 1529971200, 915408000, 915408000),
tickers = tickers
)


download_data <- function(ticker, start_period) {
  url <- sprintf(url_tmpl, ticker, start_period)
  message(sprintf("Downloading: %s from %s", ticker, url))
  download.file(url, paste0("data/", ticker, ".csv"))
  1
}
ret <- mapply(download_data, data_map$tickers,  data_map$start_period)
##################### Load Raw Data and check #######################

update_data <- function(dt, date, open, high, low, close, vol) {
  idx <- which(dt$Date==date)
  dt$Open[idx] <- open
  dt$High[idx] <- high
  dt$Low[idx] <- low
  dt$Close[idx] <- close
  dt$Volume[idx] <- vol
  dt
}

rtx_update <- function(dt) {
  dt <- update_data(dt, "2006-09-04",39.49,40.21,39.15,39.86,4499175)
  dt <- update_data(dt,  "2008-09-01", 41.81,42.76,40.08,40.33,10620225)
  dt <- update_data(dt,  "2010-01-18",45.31,45.90,43.42,43.47,7261375)
  dt <- update_data(dt,  "2011-07-04",56.55,57.79,56.15,56.86,7632750)
  dt <- update_data(dt,  "2006-01-02",35.53,35.75,34.90,35.34,5094375)
  dt <- update_data(dt,  "2012-09-03",50.16,50.56,48.77,49.97,7745225)
  dt <- update_data(dt,  "2013-01-21",54.52,56.52,54.34,56.51,7141825)
  dt <- update_data(dt,  "2013-02-18",57.29,57.59,55.71,56.95,5909550)
  dt <- update_data(dt,  "2004-09-06",29.89,29.99,29.48,29.71,5069500)
  dt <- update_data(dt,  "2005-02-21",31.59,31.88,31.04,31.79,5880600)
  dt <- update_data(dt,  "2013-05-27",60.38,60.81,59.67,59.72,5774400)
  dt <- update_data(dt,  "2005-05-30",33.82,34.00,33.36,33.43,5386400)
  dt <- update_data(dt,  "2014-01-20",72.27,74.39,70.34,70.36,7542225)
  dt <- update_data(dt,  "2019-05-27",82.84,83.75,79.00,79.48,5219625)
  dt <- update_data(dt,  "2019-01-21",71.12,75.11,69.63,72.88,12683175)
  dt <- update_data(dt,  "2002-02-18",21.88,22.15,21.42,21.93,7544600)
  dt <- update_data(dt,  "2017-01-16",69.09,70.23,68.85,69.72,4291425)
  dt <- update_data(dt,  "2016-12-26",69.88,69.94,68.84,68.99,3204250)
  dt <- update_data(dt,  "2017-01-02",69.47,71.01,69.23,70.83,4686675)
  dt <- update_data(dt,  "2017-02-20",70.60,71.04,70.31,70.77,4263275)
  dt <- update_data(dt,  "2017-09-04",72.74,72.94,68.66,68.94,13095100)
  dt <- update_data(dt,  "2018-05-28",79.40,79.70,78.18,79.18,4630300)
  dt <- update_data(dt,  "2018-02-19",81.03,84.91,79.22,83.79,9962350)
  dt <- update_data(dt,  "2018-01-15",86.68,86.68,83.67,85.53,6186550)
  dt <- update_data(dt,  "2016-09-05",67.34,67.65,64.61,64.62,5268800)
  dt <- update_data(dt,  "2016-05-30",63.42,63.69,62.15,63.20,5081800)
  dt <- update_data(dt,  "2015-05-25",74.61,74.65,73.26,73.74,5865225)
  dt <- update_data(dt,  "2017-12-25",80.21,80.86,79.87,80.28,2416575)
  dt <- update_data(dt,  "2016-07-04",64.25,65.29,62.50,65.24,6392925)
  dt <- update_data(dt,  "2018-01-01",80.49,83.30,80.03,82.80,6133350)
  dt <- update_data(dt,  "2017-05-29",76.65,77.31,76.15,76.85,4924375)
  dt <- update_data(dt,  "2018-09-03",83.04,84.70,82.15,83.08,4905875)
  dt <- update_data(dt,  "2015-09-07",57.99,59.06,57.20,58.11,8028050)
  dt <- update_data(dt,  "2016-02-15",54.97,56.04,53.85,55.53,11487125)
  dt <- update_data(dt,  "2016-01-18",54.34,54.99,52.48,54.34,10909050)
  dt <- update_data(dt,  "2001-05-28",26.43,26.49,25.67,26.17,4287125)
  dt <- update_data(dt,  "2015-01-19",73.37,76.12,72.94,75.51,9337275)
  dt <- update_data(dt,  "2008-06-30",38.48,39.11,37.48,38.42,15440150)
  dt <- update_data(dt,  "2011-04-18",51.99,54.92,51.10,54.81,9017125)
  dt <- update_data(dt,  "2018-03-26",78.00,80.67,77.68,79.18,10527625)
  dt <- update_data(dt,  "2019-04-15",84.96,86.69,84.37,86.22,3637750)
  dt <- update_data(dt,  "2017-04-10",71.11,71.59,70.37,70.54,3717025)
  dt <- update_data(dt,  "2016-03-21",62.35,62.68,61.67,62.34,5398325)
  dt <- update_data(dt,  "2015-06-29",70.95,71.13,68.74,68.95,7689300)
  dt <- update_data(dt,  "2015-12-21",58.54,60.79,58.33,60.60,7711675)
  dt <- update_data(dt,  "2015-12-28",60.40,61.53,60.23,60.46,5168500)
  dt
}

wba_update <- function(dt) {
  dt <- update_data(dt,  "2013-03-25",46.43,47.76,46.09,47.68,6907125)
  dt <- update_data(dt,  "2012-04-02",33.56,34.73,32.80,32.84,10337900)
  dt <- update_data(dt,  "2008-06-30",32.96,33.05,31.25,31.45,9194750)
  dt <- update_data(dt,  "2011-07-04",42.78,44.26,42.32,44.07,5803025)
  dt <- update_data(dt,  "2011-05-30",43.99,44.06,42.60,43.15,6106675)
  dt <- update_data(dt,  "2011-02-21",42.22,42.54,40.79,41.97,6539675)
  dt <- update_data(dt,  "2011-01-17",41.45,42.20,41.07,41.69,5792475)
  dt <- update_data(dt,  "2013-05-27",51.05,51.25,47.75,47.76,4875475)
  dt <- update_data(dt,  "2013-02-18",41.31,41.99,41.31,41.81,5046800)
  dt <- update_data(dt,  "2013-01-21",39.19,39.81,39.13,39.67,4884125)
  dt <- update_data(dt,  "2010-09-06",28.16,29.12,28.12,28.96,5271675)
  dt <- update_data(dt,  "2012-09-03",35.64,36.19,34.63,34.94,9355600)
  dt <- update_data(dt,  "2012-10-29",35.27,35.69,34.75,34.89,6403233)
  dt <- update_data(dt,  "2011-09-05",34.00,36.45,33.90,35.33,8292675)
  dt <- update_data(dt,  "2010-07-05",27.09,28.41,26.45,28.40,11238600)
  dt <- update_data(dt,  "2012-05-28",31.51,31.64,29.80,29.93,6564775)
  dt <- update_data(dt,  "2012-01-02",33.45,33.70,32.32,33.08,8397500)
  dt <- update_data(dt,  "2011-12-26",35.18,35.19,32.99,33.06,6865650)
  dt <- update_data(dt,  "2006-09-04",49.97,50.99,49.83,50.91,3826875)
  dt <- update_data(dt,  "2007-01-15",46.01,46.60,45.80,46.01,3244525)
  dt <- update_data(dt,  "2007-09-03",45.01,45.69,43.78,44.15,4493550)
  dt <- update_data(dt,  "2012-02-20",34.82,34.86,33.82,33.86,6505125)
  dt <- update_data(dt,  "2008-09-01",36.66,37.24,34.04,34.59,8439550)
  dt <- update_data(dt,  "2008-05-26",35.17,36.20,35.03,36.02,5180750)
  dt <- update_data(dt,  "2012-01-16",32.93,33.89,32.82,33.48,7049300)
  dt <- update_data(dt,  "2008-02-18",35.67,37.46,35.20,37.41,6422625)
  dt <- update_data(dt,  "2008-01-21",32.50,35.66,32.50,34.28,11171550)
  dt <- update_data(dt,  "2009-01-19",26.82,27.35,25.68,26.93,8705125)
  dt <- update_data(dt,  "2007-02-19",45.83,46.49,45.13,45.32,3727750)
  dt <- update_data(dt,  "2007-01-01",45.89,46.69,45.39,45.50,4552400)
  dt <- update_data(dt,  "2009-02-16",25.73,27.25,24.99,25.08,9621300)
  dt
}

load_data <- function(ticker) {
  dt <- read_csv(paste0("data/", ticker, ".csv"))
  if (ticker == "RTX") dt <- rtx_update(dt)
  if (ticker == "WBA") dt <- wba_update(dt)
  Chg <- 100*(dt$Close - dt$Open)/dt$Open
  dx <- cbind(dt,
              Ticker=ticker,
              Change=Chg,
              Outcome=lead(Chg, n=1),
              ROC1=(dt$Close - lag(dt$Close, n=1))/dt$Close,
              ROC2=(dt$Close - lag(dt$Close, n=2))/dt$Close
              )
  dx[4:length(dx$Close) - 1,]
}

data <- sapply(tickers, load_data)
str(data)
colnames(data)
rownames(data)

as.data.frame(data[,"MMM"]) %>% ggplot(aes(Date, Change, Ticker)) + geom_line() + ylab("MMM Change")
as.data.frame(data[,"AXP"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("AXP Change")
as.data.frame(data[,"AAPL"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("AAPL Change")
as.data.frame(data[,"BA"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("BA Change")
as.data.frame(data[,"CAT"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("CAT Change")
as.data.frame(data[,"CVX"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("CVX Change")
as.data.frame(data[,"CSCO"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("CSCO Change")
as.data.frame(data[,"KO"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("KO Change")
as.data.frame(data[,"DOW"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("DOW Change")
as.data.frame(data[,"XOM"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("XOM Change")
as.data.frame(data[,"GS"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("GS Change")
as.data.frame(data[,"HD"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("HD Change")
as.data.frame(data[,"IBM"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("IBM Change")
as.data.frame(data[,"INTC"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("INTC Change")
as.data.frame(data[,"JNJ"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("JNJ Change")
as.data.frame(data[,"JPM"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("JPM Change")
as.data.frame(data[,"MCD"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("MCD Change")
as.data.frame(data[,"MRK"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("MRK Change")
as.data.frame(data[,"MSFT"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("MSFT Change")
as.data.frame(data[,"NKE"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("NKE Change")
as.data.frame(data[,"PFE"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("PFE Change")
as.data.frame(data[,"PG"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("PG Change")
as.data.frame(data[,"RTX"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("RTX Change")
as.data.frame(data[,"TRV"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("TRV Change")
as.data.frame(data[,"UNH"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("UNH Change")
as.data.frame(data[,"VZ"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("VZ Change")
as.data.frame(data[,"V"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("V Change")
as.data.frame(data[,"WMT"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("WMT Change")
as.data.frame(data[,"WBA"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("WBA Change")
as.data.frame(data[,"DIS"]) %>% ggplot(aes(Date, Change)) + geom_line() + ylab("DIS Change")

##################### Add Outcome Rank ##############################
# Returns Date,Col dataframe
get_date_field <- function(ticker, col) {
  dat2 <- bind_cols(data["Date", ticker], data[col, ticker])
  colnames(dat2) <- c("Date", paste(ticker, col, sep="_"))
  dat2
}

# Returns a data frame of Date, Col for all Tickers
get_combined_field_data <- function(col) {
  datr <- data.frame(stringsAsFactors = FALSE)
  for (ticker in tickers) {
    if (length(datr) == 0) {
      datr <- get_date_field(ticker, col)
    }
    else {
      datr <- full_join(datr, get_date_field(ticker, col), by="Date")
    }
  }
  datr
}

# Find the ranks of the data points in each row and appends an extension with Rank column name
get_ranks <- function(col) {
  dat1 <- get_combined_field_data(col)
  n_col <- ncol(dat1)
  rank_data <- as.integer(rank(-dat1[1,2:n_col], na.last="keep"))
  for (i in 2:nrow(dat1)) {
    rank_data <- rbind(rank_data, as.integer(rank(-dat1[i,2:n_col], na.last="keep")))
  }
  colnames(rank_data) <- str_replace(colnames(dat1)[2:n_col], col, "Rank")
  rank_data
}

denorm_data_with_ranks <- function(outcome_rank, change_rank) {
  dedata <- data.frame()
  for (ticker in tickers) {
    print(ticker)
    x <- as.data.frame(data[,ticker])
    or <- na.omit(outcome_rank[,paste0(ticker,"_Rank")])
    or_1 <- c(or[2:length(or)], NA)
    or_2 <- c(or[3:length(or)], NA, NA)
    x <- mutate(x, Rank=or)
    x <- mutate(x, Rank_1=or_1)
    x <- mutate(x, Rank_2=or_2)
    cr <- na.omit(change_rank[,paste0(ticker,"_Rank")])
    x <- mutate(x, ChangeRank=cr)
    if (length(dedata) == 0) {
      dedata <- x[1:(length(x$Rank) - 2),]
    }
    else {
      dedata <- bind_rows(dedata, x[1:(length(x$Rank) - 2),])
    }

  }
  dedata
}

outcome_rank <- get_ranks("Outcome")
change_rank <- get_ranks("Change")

dedata <- denorm_data_with_ranks(outcome_rank, change_rank)
head(dedata)
######################### Visualization #############################
dedata %>% filter(year(Date) == 1999) %>% ggplot(aes(Date, Outcome, color=Ticker)) + geom_line() + ylab("1999 Outcome")
dedata %>% filter(year(Date) == 2000) %>% ggplot(aes(Date, Outcome, color=Ticker)) + geom_line() + ylab("2000 Outcome")
dedata %>% filter(year(Date) == 2001) %>% ggplot(aes(Date, Outcome, color=Ticker)) + geom_line() + ylab("2001 Outcome")
dedata %>% filter(year(Date) == 2002) %>% ggplot(aes(Date, Outcome, color=Ticker)) + geom_line() + ylab("2002 Outcome")
dedata %>% filter(year(Date) == 2003) %>% ggplot(aes(Date, Outcome, color=Ticker)) + geom_line() + ylab("2003 Outcome")
dedata %>% filter(year(Date) == 2004) %>% ggplot(aes(Date, Outcome, color=Ticker)) + geom_line() + ylab("2004 Outcome")
dedata %>% filter(year(Date) == 2005) %>% ggplot(aes(Date, Outcome, color=Ticker)) + geom_line() + ylab("2005 Outcome")
dedata %>% filter(year(Date) == 2006) %>% ggplot(aes(Date, Outcome, color=Ticker)) + geom_line() + ylab("2006 Outcome")
dedata %>% filter(year(Date) == 2007) %>% ggplot(aes(Date, Outcome, color=Ticker)) + geom_line() + ylab("2007 Outcome")
dedata %>% filter(year(Date) == 2008) %>% ggplot(aes(Date, Outcome, color=Ticker)) + geom_line() + ylab("2008 Outcome")
dedata %>% filter(year(Date) == 2009) %>% ggplot(aes(Date, Outcome, color=Ticker)) + geom_line() + ylab("2009 Outcome")
dedata %>% filter(year(Date) == 2010) %>% ggplot(aes(Date, Outcome, color=Ticker)) + geom_line() + ylab("2010 Outcome")
dedata %>% filter(year(Date) == 2011) %>% ggplot(aes(Date, Outcome, color=Ticker)) + geom_line() + ylab("2011 Outcome")
dedata %>% filter(year(Date) == 2012) %>% ggplot(aes(Date, Outcome, color=Ticker)) + geom_line() + ylab("2012 Outcome")
dedata %>% filter(year(Date) == 2013) %>% ggplot(aes(Date, Outcome, color=Ticker)) + geom_line() + ylab("2013 Outcome")
dedata %>% filter(year(Date) == 2014) %>% ggplot(aes(Date, Outcome, color=Ticker)) + geom_line() + ylab("2014 Outcome")
dedata %>% filter(year(Date) == 2015) %>% ggplot(aes(Date, Outcome, color=Ticker)) + geom_line() + ylab("2015 Outcome")
dedata %>% filter(year(Date) == 2016) %>% ggplot(aes(Date, Outcome, color=Ticker)) + geom_line() + ylab("2016 Outcome")
dedata %>% filter(year(Date) == 2017) %>% ggplot(aes(Date, Outcome, color=Ticker)) + geom_line() + ylab("2017 Outcome")
dedata %>% filter(year(Date) == 2018) %>% ggplot(aes(Date, Outcome, color=Ticker)) + geom_line() + ylab("2018 Outcome")
dedata %>% filter(year(Date) == 2019) %>% ggplot(aes(Date, Outcome, color=Ticker)) + geom_line() + ylab("2019 Outcome")

# Lets check if there is any pattern in rank
dedata %>% filter(year(Date) == 1999) %>% ggplot(aes(Date, Rank, color=Ticker)) + geom_line() + ylab("1999 Rank")
dedata %>% filter(year(Date) == 2000) %>% ggplot(aes(Date, Rank, color=Ticker)) + geom_line() + ylab("2000 Rank")
dedata %>% filter(year(Date) == 2001) %>% ggplot(aes(Date, Rank, color=Ticker)) + geom_line() + ylab("2001 Rank")
dedata %>% filter(year(Date) == 2002) %>% ggplot(aes(Date, Rank, color=Ticker)) + geom_line() + ylab("2002 Rank")
dedata %>% filter(year(Date) == 2003) %>% ggplot(aes(Date, Rank, color=Ticker)) + geom_line() + ylab("2003 Rank")
dedata %>% filter(year(Date) == 2004) %>% ggplot(aes(Date, Rank, color=Ticker)) + geom_line() + ylab("2004 Rank")
dedata %>% filter(year(Date) == 2005) %>% ggplot(aes(Date, Rank, color=Ticker)) + geom_line() + ylab("2005 Rank")
dedata %>% filter(year(Date) == 2006) %>% ggplot(aes(Date, Rank, color=Ticker)) + geom_line() + ylab("2006 Rank")
dedata %>% filter(year(Date) == 2007) %>% ggplot(aes(Date, Rank, color=Ticker)) + geom_line() + ylab("2007 Rank")
dedata %>% filter(year(Date) == 2008) %>% ggplot(aes(Date, Rank, color=Ticker)) + geom_line() + ylab("2008 Rank")
dedata %>% filter(year(Date) == 2009) %>% ggplot(aes(Date, Rank, color=Ticker)) + geom_line() + ylab("2009 Rank")
dedata %>% filter(year(Date) == 2010) %>% ggplot(aes(Date, Rank, color=Ticker)) + geom_line() + ylab("2010 Rank")
dedata %>% filter(year(Date) == 2011) %>% ggplot(aes(Date, Rank, color=Ticker)) + geom_line() + ylab("2011 Rank")
dedata %>% filter(year(Date) == 2012) %>% ggplot(aes(Date, Rank, color=Ticker)) + geom_line() + ylab("2012 Rank")
dedata %>% filter(year(Date) == 2013) %>% ggplot(aes(Date, Rank, color=Ticker)) + geom_line() + ylab("2013 Rank")
dedata %>% filter(year(Date) == 2014) %>% ggplot(aes(Date, Rank, color=Ticker)) + geom_line() + ylab("2014 Rank")
dedata %>% filter(year(Date) == 2015) %>% ggplot(aes(Date, Rank, color=Ticker)) + geom_line() + ylab("2015 Rank")
dedata %>% filter(year(Date) == 2016) %>% ggplot(aes(Date, Rank, color=Ticker)) + geom_line() + ylab("2016 Rank")
dedata %>% filter(year(Date) == 2017) %>% ggplot(aes(Date, Rank, color=Ticker)) + geom_line() + ylab("2017 Rank")
dedata %>% filter(year(Date) == 2018) %>% ggplot(aes(Date, Rank, color=Ticker)) + geom_line() + ylab("2018 Rank")
dedata %>% filter(year(Date) == 2019) %>% ggplot(aes(Date, Rank, color=Ticker)) + geom_line() + ylab("2019 Rank")

# Check Ranks for each Ticker lifespan
dedata %>% filter(Ticker == "MMM") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("MMM Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "AXP") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("AXP Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "AAPL") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("AAPL Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "BA") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("BA Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "CAT") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("CAT Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "CVX") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("CVX Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "CSCO") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("CSCO Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "KO") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("KO Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "DOW") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("DOW Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "XOM") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("XOM Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "GS") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("GS Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "HD") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("HD Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "IBM") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("IBM Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "INTC") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("INTC Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "JNJ") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("JNJ Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "JPM") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("JPM Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "MCD") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("MCD Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "MRK") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("MRK Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "MSFT") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("MSFT Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "NKE") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("NKE Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "PFE") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("PFE Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "PG") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("PG Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "RTX") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("RTX Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "TRV") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("TRV Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "UNH") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("UNH Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "VZ") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("VZ Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "V") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("V Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "WMT") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("WMT Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "WBA") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("WBA Rank") + geom_abline(intercept = 1,slope = 0)
dedata %>% filter(Ticker == "DIS") %>% ggplot(aes(Date, Rank)) + geom_line() + ylab("DIS Rank") + geom_abline(intercept = 1,slope = 0)

# Influence of current week's rank vs next week's rank
t_rank_change_rank <- table(dedata %>% select(ChangeRank, Rank))
t_rank_change_rank[,1]
# Lets check the distribution of Rank 1 vs ChangeRank
as.data.frame(t_rank_change_rank[,1]) %>%
        ggplot(aes(1:30, t_rank_change_rank[,1])) +
        geom_point() +
        ylab("Frequency of Rank 1 Next Week") +
        xlab("Today's Rank")
as.data.frame(t_rank_change_rank[,2]) %>%
        ggplot(aes(1:30, t_rank_change_rank[,2])) +
        geom_point() +
        ylab("Frequency of Rank 2 Next Week") +
        xlab("Today's Rank")
as.data.frame(t_rank_change_rank[,3]) %>%
        ggplot(aes(1:30, t_rank_change_rank[,3])) +
        geom_point() +
        ylab("Frequency of Rank 3 Next Week") +
        xlab("Today's Rank")

# THe 3 graphs above shows negative co-relation beterrn ChangeRank and Rank. Implies lower the change rank higher is
# the probability to land among high rank next day

## Does today's change effect tomorrow's rank
head(dedata)
boxplot(Change~Rank,data=dedata)
# Very interesting graph suggesting that Change as no effect on the rank of next day
cor(dedata$Outcome, dedata$Rank)
cor(dedata$Outcome, dedata$Rank_1)
cor(dedata$Outcome, dedata$Rank_2)
cor(dedata$Outcome, dedata$ROC1)
cor(dedata$Outcome, dedata$ROC2)
cor(dedata$Rank, dedata$ROC1)
cor(dedata$Rank, dedata$ROC2)
save(dedata, file="Data.Rda")
########### Model Building
set.seed(1, sample.kind="Rounding")
load("Data.Rda")

# 60% train and 40% test set
test_index <- createDataPartition(y = dedata$Outcome, times = 1, p = 0.4, list = FALSE)
train_set <- dedata[-test_index,]
test_set <- dedata[test_index,]

# Timed Train and Test Set
timed_train_set <- dedata %>% filter(year(Date) < 2012)
timed_test_set <- dedata %>% filter(year(Date) >= 2012)

### Linear Regression
#### Random Set
fit_lm_1_r <- lm(Outcome ~ Rank, data = train_set)
fit_lm_2_1_r <- lm(Outcome ~ Rank + ROC1, data = train_set)
fit_lm_2_2_r <- lm(Outcome ~ Rank + ROC2, data = train_set)
fit_lm_2_3_r <- lm(Outcome ~ Rank + Rank_1, data = train_set)
fit_lm_2_4_r <- lm(Outcome ~ Rank + Rank_2, data = train_set)
fit_lm_3_1_r <- lm(Outcome ~ Rank + ROC1 + ROC2, data = train_set)
fit_lm_3_2_r <- lm(Outcome ~ Rank + ROC1 + Rank_1, data = train_set)
fit_lm_3_3_r <- lm(Outcome ~ Rank + ROC1 + Rank_2, data = train_set)
fit_lm_3_4_r <- lm(Outcome ~ Rank + Rank_1 + ROC2, data = train_set)
fit_lm_3_5_r <- lm(Outcome ~ Rank + Rank_2 + ROC2, data = train_set)
fit_lm_3_6_r <- lm(Outcome ~ Rank + Rank_1 + Rank_2, data = train_set)
fit_lm_5_1_r <- lm(Outcome ~ Rank + Rank_1 + Rank_2 + ROC1 + ROC2, data = train_set)

#### Timed Set
fit_lm_5_1_t  <- lm(Outcome ~ Rank + Rank_1 + Rank_2 + ROC1 + ROC2, data = timed_train_set)


### Random Forest
#### Random Set
fit_rf_1_r <- train(Outcome ~ Rank, method = "rf", tuneGrid = data.frame(mtry = seq(50, 200, 25)), data=train_set,
                    ntree=100)
fit_rf_5_1_r <- train(Outcome ~ Rank + Rank_1 + Rank_2 + ROC1 + ROC2, method = "rf",
                      tuneGrid = data.frame(mtry = seq(50, 200, 25)), data=train_set, ntree=100)
#### Timed Set
fit_rf_5_1_t <- train(Outcome ~ Rank + Rank_1 + Rank_2 + ROC1 + ROC2, method = "rf",
                      tuneGrid = data.frame(mtry = seq(50, 200, 25)), data=timed_train_set, ntree=100)

### XGBoost
#### Random Set
fit_xgb_1_r <- train(Outcome ~ Rank, method = "xgbTree", data=train_set)
fit_xgb_5_1_r <- train(Outcome ~ Rank + Rank_1 + Rank_2 + ROC1 + ROC2, method = "xgbTree", data=train_set)

#### Timed Set
fit_xgb_5_1_t <- train(Outcome ~ Rank + Rank_1 + Rank_2 + ROC1 + ROC2, method = "xgbTree", data=timed_train_set)

########### Model Prediction
### Linear Regression
#### Random Set
hat_lm_1_r <- predict(fit_lm_1_r, newdata = test_set)
hat_lm_5_1_r <- predict(fit_lm_5_1_r, newdata = test_set)
#### Timed Set
hat_lm_5_1_t <- predict(fit_lm_5_1_t, newdata = timed_test_set)

### Random Forest
#### Random Set
hat_rf_1_r <- predict(fit_rf_1_r, newdata = test_set)
hat_rf_5_1_r <- predict(fit_rf_5_1_r, newdata = test_set)
#### Timed Set
hat_rf_5_1_t <- predict(fit_rf_5_1_t, newdata = timed_test_set)

### XGBoost
#### Random Set
hat_xgb_1_r <- predict(fit_xgb_1_r, newdata = test_set)
hat_xgb_5_1_r <- predict(fit_xgb_5_1_r, newdata = test_set)
#### Timed Set
hat_xgb_5_1_t <- predict(fit_xgb_5_1_t, newdata = timed_test_set)

######### Model Evaluation
save(fit_lm_5_1_t, fit_rf_5_1_t, fit_xgb_5_1_t, hat_lm_5_1_t, hat_rf_5_1_t, hat_xgb_5_1_t, timed_train_set,
     timed_test_set, file="tmodels.Rda")
load("tmodels.Rda")
### Linear Regression
#### Random Set
cor(hat_lm_1_r, test_set$Outcome)
cor(hat_lm_2_1_r, test_set$Outcome)
cor(hat_lm_2_2_r, test_set$Outcome)
cor(hat_lm_2_3_r, test_set$Outcome)
cor(hat_lm_2_4_r, test_set$Outcome)
cor(hat_lm_3_1_r, test_set$Outcome)
cor(hat_lm_3_2_r, test_set$Outcome)
cor(hat_lm_3_3_r, test_set$Outcome)
cor(hat_lm_3_4_r, test_set$Outcome)
cor(hat_lm_3_5_r, test_set$Outcome)
cor(hat_lm_3_6_r, test_set$Outcome)
cor(hat_lm_5_1_r, test_set$Outcome)
#### Timed Set
cor(hat_lm_5_1_t, timed_test_set$Outcome)

# Add Prediticted Outcome in the test set and generate rank per date
td <- cbind(timed_test_set, PredictedOutcome=hat_lm_5_1_t) %>%
      group_by(Date) %>%
      mutate(PredictedRank=order(PredictedOutcome, decreasing=TRUE))

# Find the outcome rank 1 overall and by year
rank1_res <- td %>% filter(PredictedRank == 1) %>% select(Date, Ticker, Rank, PredictedRank, Outcome)
# Profit per Rank
td %>% group_by(PredictedRank) %>% summarize(sum(Outcome)) %>% print(n=30)
# Aggregrate Total Profit
sum(rank1_res$Outcome)
# Profit per Year
rank1_res %>% group_by(year(Date)) %>% summarize(sum(Outcome))

### Random Forest
#### Random Set
cor(hat_rf_1_r, test_set$Outcome)
cor(hat_rf_5_1_r, test_set$Outcome)
#### Timed Set
cor(hat_rf_5_1_t, timed_test_set$Outcome)
# Add Prediticted Outcome in the test set and generate rank per date
trf <- cbind(timed_test_set, PredictedOutcome=hat_rf_5_1_t) %>%
      group_by(Date) %>%
      mutate(PredictedRank=order(PredictedOutcome, decreasing=TRUE))

# Find the outcome rank 1 overall and by year
rank1_rf <- trf %>% filter(PredictedRank == 1) %>% select(Date, Ticker, Rank, PredictedRank, Outcome)
# Profit per Rank
trf %>% group_by(PredictedRank) %>% summarize(sum(Outcome)) %>% print(n=30)
# Aggregrate Total Profit
sum(rank1_rf$Outcome)
# Profit per Year
rank1_rf %>% group_by(year(Date)) %>% summarize(sum(Outcome))

### XGBoost
#### Random Set
cor(hat_xgb_1_r, test_set$Outcome)
cor(hat_xgb_5_1_r, test_set$Outcome)
#### Timed Set
cor(hat_xgb_5_1_t, timed_test_set$Outcome)
# Add Prediticted Outcome in the test set and generate rank per date
txgb <- cbind(timed_test_set, PredictedOutcome=hat_xgb_5_1_t) %>%
      group_by(Date) %>%
      mutate(PredictedRank=order(PredictedOutcome, decreasing=TRUE))

# Find the outcome rank 1 overall and by year
rank1_xgb <- txgb %>% filter(PredictedRank == 1) %>% select(Date, Ticker, Rank, PredictedRank, Outcome)
# Profit per Rank
txgb %>% group_by(PredictedRank) %>% summarize(sum(Outcome)) %>% print(n=30)
# Aggregrate Total Profit
sum(rank1_xgb$Outcome)
# Profit per Year
rank1_xgb %>% group_by(year(Date)) %>% summarize(sum(Outcome))

# Frequency of prediction of Rank 1 stock
rank1_res %>% ggplot(aes(Rank)) +
  geom_histogram() +
  ylab("Predicted Rank 1 Frequency") +
  xlab("Actual Rank") +
  ggtitle("Predicted Rank 1 vs Actual Rank Frequency")
rank1_rf %>% ggplot(aes(Rank)) +
  geom_histogram() +
  ylab("Predicted Rank 1 Frequency") +
  xlab("Actual Rank") +
  ggtitle("Predicted Rank 1 vs Actual Rank Frequency")
rank1_xgb %>% ggplot(aes(Rank)) +
  geom_histogram() +
  ylab("Predicted Rank 1 Frequency") +
  xlab("Actual Rank") +
  ggtitle("Predicted Rank 1 vs Actual Rank Frequency")
