# Libraries and options ---------------------------------------------------

library(data.table)
library(Rbitcoin)
library(rbitcoinchartsapi)
library(lubridate)

# # Choose here:
# options( scipen = 999 )
# options( scipen = 0 )






# Start accumulating daily data for analysis -------------------------------------------

# Always go for one day in the past, where there is full data already.
# Then run this bit daily at some arbitrary time.

yesterday <- as.Date( Sys.time( ) ) - 1

historicTradeData_GBP_yesterday <- rbitcoinchartsapi::GetHistoricTradeData( list( symbol = "localbtcGBP", 
                                                                                  start = as.numeric( as.POSIXct( yesterday, 
                                                                                                                  format="%Y-%m-%d" ) ) ) )
setDT( historicTradeData_GBP_yesterday )[  , unixtime := as.POSIXct( unixtime, origin = "1970-01-01" ) ]
historicTradeData_GBP_yesterday <- historicTradeData_GBP_yesterday[ unixtime < as.Date( Sys.time() ), ]




historicTradeData_EUR_yesterday <- rbitcoinchartsapi::GetHistoricTradeData( list( symbol = "localbtcEUR", 
                                                                                  start = as.numeric( as.POSIXct( yesterday, 
                                                                                                                  format="%Y-%m-%d" ) ) ) )
setDT( historicTradeData_EUR_yesterday )[  , unixtime := as.POSIXct( unixtime, origin = "1970-01-01" ) ]
historicTradeData_EUR_yesterday <- historicTradeData_EUR_yesterday[ unixtime < as.Date( Sys.time() ), ]


save.image( paste( "/home/caterina/Documents/TDL_Internal_Projects/Data_Team_Internal_Projects/BitcoinProject/DailyBitcoinPrices_", 
                   yesterday,
                   ".RData", 
                   sep = "" ) )


# Here's an option that then incorporates several existing RData files, once I accumulate a bunch.
# all.files = c("file1.RData", "file2.RData", "file3.RData")

# This script will be run by Ubuntu every dat at 7.15 am, via kde-config-cron.

