#################### Building Universe ######################

# Preliminaries


library(rstudioapi)  # This is a external library of functions
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
cat("\014")


install.packages(Quandl)
library(Quandl)                         ############ To import data from Quandl
Quandl.api_key('EfNYF1EymebW8saMFp5B')


############### Import symbols and remove duplicates


symbols <- as.vector(read.csv("SP Tickers.csv")[,1])
symbols <- unique(symbols)

############### Import Stocks data from Quandl

firsttime<-TRUE
for (i in symbols){
  print(i)
  temp <- Quandl.datatable("SHARADAR/SEP",date.gte="2018-02-01",ticker=i)
  if (firsttime == TRUE) {
    firsttime <- FALSE
    stock <- temp
  } else {
    stock <- rbind(stock,temp)
  }
}

########### Dealing with Additions - Retain data only after the date this Holding was added to S&P 500

add <- read.csv("SP Additions.csv")

add$date.added <- as.Date(add$date.added,format="%m/%d/%Y")
names(stock)[1] <- "symbol"
temp <- merge(stock,add,all.x = T)

temp$date.added <- as.Date(ifelse(is.na(temp$date.added),as.Date("2018-02-01"),temp$date.added))
temp <- subset(temp,temp$date>=temp$date.added)


########## Dealing with Removals - Remove data for any stock that was removed from S&P 500. Retain only before removal

remove <- read.csv("SP Removals.csv")

remove$date.removed <- as.Date(remove$date.removed,format="%m/%d/%Y")
#names(stock)[1] <- "symbol"
temp <- merge(temp,remove,all.x = T)

temp$date.removed <- as.Date(ifelse(is.na(temp$date.removed),as.Date("2024-02-01"),temp$date.removed))
temp <- subset(temp,temp$date<=temp$date.removed)

temp<-temp[order(temp$symbol,temp$date),]

################ Store required data in stock and save it

stock <- temp[,c(1:9)]
rownames(stock)<-seq(1,nrow(stock),1)
save(stock,file='universe.rdata')

#rm(list(temp))
