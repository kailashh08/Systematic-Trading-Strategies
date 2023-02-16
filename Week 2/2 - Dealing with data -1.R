################################################################################
# PART-1 #
################################################################################

# Preliminaries


library(rstudioapi)  # This is a external library of functions
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
cat("\014")


library(dplyr)


Quandl.api_key('EfNYF1EymebW8saMFp5B')


################## Remove duplicates in Symbols and read data from Quandl ######################

firsttime <- T
symbols <- as.vector(read.csv("SP Tickers.csv")[,1])
symbols <- unique(symbols)

for (i in symbols){
  print(i)
  test <- Quandl.datatable("SHARADAR/SF2",filingdate.gte="2017-02-01",ticker=i,paginate = T)
  if (firsttime == T) {
    firsttime <- F
    insider <- test
  } else  {
    insider <- rbind(insider,test)
  }
}

################## Filter for Transaction Date, Sort the table ######################

insider <- subset(insider,transactiondate>="2018-02-01")

insider <- insider[order(insider$ticker,insider$filingdate,insider$ownername),]
rownames(insider)<- seq(1,nrow(insider),1)


################## Change N/As to 0, Add new column for Buy and Sell ######################

insider$transactionshares <- ifelse(is.na(insider$transactionshares),0,insider$transactionshares)
insider$transactionvalue <- ifelse(is.na(insider$transactionvalue),0,insider$transactionvalue)

insider$buy.sell <- ifelse(insider$transactionshares<0,"Sell","Buy")



##################  Group by and sum up No of shares,                                                   #####################
##################  pivot(Cross tab) buy/sell to get Shares bought and shares sold on different columns ######################

Agg1 <- insider %>%
            group_by(ticker,transactiondate,buy.sell) %>%
            summarise(shares = sum(transactionshares))

Pivot1 <- Agg1 %>% 
  group_by(ticker,transactiondate,buy.sell) %>%
  spread(buy.sell,shares)

colnames(Pivot1)[3:4] <- c("Shares_Bought","Shares_Sold")

################## Same as above, but for transaction value ######################

Agg2 <- insider %>%
  group_by(ticker,transactiondate,buy.sell) %>%
  summarise(amount = sum(transactionvalue))

Pivot2 <- Agg2 %>% 
  group_by(ticker,transactiondate,buy.sell) %>%
  spread(buy.sell,amount)

colnames(Pivot2)[3:4] <- c("Amount_Bought","Amount_Sold")

################## Same as above, but for Number of buyers and sellers ######################

Agg3 <- insider %>%
  group_by(ticker,transactiondate,buy.sell) %>%
  summarise(owners = n_distinct(ownername))

Pivot3 <- Agg3 %>% 
  group_by(ticker,transactiondate,buy.sell) %>%
  spread(buy.sell,owners)

colnames(Pivot3)[3:4] <- c("No_of_Buyers","No_of_Sellers")

################## Merge all data to get Insider data in one table ######################

Insider_Data <- merge(Pivot1,Pivot2)
Insider_Data <- merge(Insider_Data,Pivot3)

#Insider_Data <- reduce(function(df1,df2) merge(df1,df2),list(Pivot1,Pivot2,Pivot3))  #### Another way to do the same

Insider_Data[is.na(Insider_Data)] <- 0
colnames(Insider_Data)[1] <- "symbol"

################## Load Stock Universe and remove duplicates ######################

load("universe.rdata")

stock <- stock[!duplicated(stock),]

################## Merge stock universe with Insider data ######################

stock_with_insider <- merge(stock,Insider_Data,by.x=c("symbol","date"),
                            by.y=c("symbol","transactiondate"),all.x = T)

save(stock_with_insider,file="stock_with_insider_Kailash.rdata")
save(Insider_Data,file="Insider_Data_Kailash.rdata")
save(stock,file="stock_without_dups_Kailash.rdata")


########### Debugging code ########

#test1 <- merge(stock_with_insider,Insider_Data,all.Y = T)
#head(paste(stock_with_insider$symbol,"_",stock_with_insider$date),10)
#test1 <- subset(Insider_Data,(paste(Insider_Data$symbol,"_",Insider_Data$transactiondate))
#       %in% paste(stock_with_insider$symbol,"_",stock_with_insider$date))

#length(unique(paste(Insider_Data$symbol,"_",Insider_Data$transactiondate)))
#length(unique(paste(stock$symbol,"_",stock$date)))

#debug1 <- subset(stock %>%
#  group_by (symbol,date) %>%
#  summarize(count = n()), count!=1)
