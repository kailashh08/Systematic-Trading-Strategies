################################################################################
# PART-2 #
################################################################################

# Preliminaries

library(rstudioapi)  # This is a external library of functions
library(dplyr)
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
cat("\014")
install.packages(Quandl)
library(Quandl)                      
Quandl.api_key('EfNYF1EymebW8saMFp5B')

################  Load data and remove duplicates if any #########
load("universe.rdata") 
stock <- stock[!duplicated(stock),]

symbols <- as.vector(read.csv("SP Tickers.csv")[,1])
symbols <- unique(symbols)
#fromdate=as.Date("2018-02-01")

###########  Load Daily data from Quandl 

firsttime<-TRUE
for (currsymbol in symbols) {
  print(c(currsymbol))
  temp<-tryCatch({
    temp<-Quandl.datatable("SHARADAR/DAILY",date.gte = "2018-02-01",ticker=currsymbol)   # Use tryCatch to handle the error
  }, warning=function(w) {temp<-NULL }, error=function(e) {temp<-NULL})
  if (!is.null(temp)) {
    if (firsttime) {
      daily<-temp
    } else {
      daily<-rbind(daily,temp)}
    firsttime<-FALSE
  }
  else{
    print("hi");
  }
}

save(daily,file="daily_metric.rdata")

#load("daily_metric.rdata")


daily<-daily[order(daily$ticker,daily$date),]
daily<-daily %>% rename(symbol = ticker)

#length(unique(paste(daily$symbol,"_",daily$date))) ## 753524, no duplicates, can merge

########### Merge stock universe with Daily data to compare market cap  #################
###########         Calculate outstanding shares                        ################

stockxdm <- merge(stock[,-c(4,5)],daily,all = TRUE, na.action = "na.pass")

stockxdm$outstanding_shares_adj <- stockxdm$marketcap/stockxdm$closeadj
stockxdm$outstanding_shares_unadj <- stockxdm$marketcap/stockxdm$closeunadj


############# Reorder fields  #################

stockxdm <- stockxdm %>% relocate(marketcap,.after="lastupdated")
stockxdm <- stockxdm %>% relocate(outstanding_shares_adj,.after="marketcap")
stockxdm <- stockxdm %>% relocate(outstanding_shares_unadj,.after="outstanding_shares_adj")

############  Data is ready to be analysed #########