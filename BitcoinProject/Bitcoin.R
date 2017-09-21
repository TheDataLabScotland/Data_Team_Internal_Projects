
# Libraries and options ---------------------------------------------------

# install.packages( c( "plyr", "data.table", "lubridate", "stringr, "Rbitcoin", "rbitcoinchartsapi", "igraph", "ggplot2", "tseries", "xts" ) )

library(plyr)

library(data.table)
library(lubridate)
library(stringr)

library(Rbitcoin)
library(rbitcoinchartsapi)

library(igraph)
library(ggplot2)

library(tseries)

library(xts)
# # Choose here:
# options( scipen = 999 )
# options( scipen = 0 )




# Rbitcoin package -------------------------------------------------------


# Source: https://jangorecki.gitlab.io/Rbitcoin/library/Rbitcoin/doc/introduction.html

# Public API calls do not require any authentication and you can query it without having an account on market.


ticker <- Rbitcoin::market.api.process( market = "kraken", 
                                        currency_pair = c( "BTC","EUR" ), 
                                        action = "ticker" )
trades <- Rbitcoin::market.api.process( market = "kraken", 
                                        currency_pair = c( "BTC","EUR" ), 
                                        action = "trades" )
order_book <- Rbitcoin::market.api.process( market = "kraken", 
                                            currency_pair = c( "BTC","EUR" ), 
                                            action = "order_book" )

trades[["trades"]][ , tail( .SD, 10 ) ] # get last 10 rows from object
order_book[[ "asks" ]][ , head( .SD, 10 ) ]
order_book[[ "bids" ]][ , head( .SD, 10 ) ]

Rbitcoin.plot( trades, col = 'blue' )
Rbitcoin.plot( order_book )


ggplot( trades$trades, aes( date, price, size = amount ) ) + 
  geom_point() + 
  geom_smooth( aes( weight = amount ) ) +
  labs( x = "Time", y = "Price", color = "Symbol", size = "Volume" ) +
  guides( size = guide_legend( override.aes = list( linetype = 0 ) ) ) +
  ggtitle( "BTC - EUR" )


# Streams of data from order book ( bids and asks ) seem to be independent - no timestamp to match cases on... Not very clear to me why?
# However, to get an idea for what tendencies seem to be occurring for asks, with reference to bids... we can plot them together:
# Can also add in actual trades - which presumably are bids/asks that managed to converge and actually went through:

trades[["trades"]][ , Source := "Trades" ]
order_book[[ "asks" ]][ , Source := "Asks" ]
order_book[[ "bids" ]][ , Source := "Bids" ]

bids_asks_trades <- rbind( trades[["trades"]][ , .SD, .SDcol = c( "price", "Source" ) ],
                           order_book[[ "asks" ]][ , .SD, .SDcol = c( "price", "Source" ) ], 
                           order_book[[ "bids" ]][ , .SD, .SDcol = c( "price", "Source" ) ] )

ggplot( bids_asks_trades, aes( x = price, group = Source, fill = Source ) ) + 
  geom_histogram( color = "black", alpha = 0.4, position = "identity" ) +
  ggtitle( "(Mis)matches between trade asks, bids and done deals: BTN - EUR")



# These lead to errors, despite being taken from the package examples:
# See ?market.api.process
# action = "wallet"
# action = "open_orders"
# action = "cancel_orders" 
# action = "place_limit_order"
# Argument = 'raw.query.res' : logical skip post-processing are return results only after fromJSON processing. Useful in case of change results structure from market API. It can always be manually post-processed as a workaround till the Rbitcoin update.





# Package rbitcoinchartsapi -----------------------------------------------



# Source: https://coherentlogic.com/wordpress/middleware-development/r-package-for-the-bitcoincharts-com-api/

?GetMarketData
?GetHistoricTradeData

weightedPrices <- rbitcoinchartsapi::GetWeightedPrices()
weightedPrices$GBP

gbp <- rbitcoinchartsapi::GetMarketData( list( currency = "GBP" ) )
# See here for other params: https://bitcoincharts.com/about/markets-api/
gbp_indices <- sapply( gbp, "[[", "currency" ) == "GBP"
gbp[ gbp_indices ]


# Getting historic data starting from a specific start date - BUT it does not accept a date further back into the past than 5 days... :
# Using this, could figure out a day to scan data every 5 days, and add it to models. Or more often, if we want to.
historicTradeData_GBP <- rbitcoinchartsapi::GetHistoricTradeData( list( symbol = "localbtcGBP", 
                                                                        start = as.numeric( as.POSIXct( "2017-09-10", 
                                                                                                        format="%Y-%m-%d" ) ) ) )
historicTradeData_EUR <- rbitcoinchartsapi::GetHistoricTradeData( list( symbol = "localbtcEUR", 
                                                                        start = as.numeric( as.POSIXct( "2017-09-10", 
                                                                                                        format="%Y-%m-%d" ) ) ) )
historicTradeData_CNY <- rbitcoinchartsapi::GetHistoricTradeData( list( symbol = "btctradeCNY",
                                                                        start = as.numeric( as.POSIXct( "2017-09-10", 
                                                                                                        format="%Y-%m-%d" ) ) ) )

setDT( historicTradeData_GBP )[ , symbol := "GBP" ]
setDT( historicTradeData_EUR )[ , symbol := "EUR" ]
setDT( historicTradeData_CNY )[ , symbol := "CNY" ]

historicTradeData_GBP[  , unixtime := as.POSIXct( unixtime, origin = "1970-01-01" ) ]
historicTradeData_EUR[  , unixtime := as.POSIXct( unixtime, origin = "1970-01-01" ) ]
historicTradeData_CNY[  , unixtime := as.POSIXct( unixtime, origin = "1970-01-01" ) ]

historicTradeData <- rbind( historicTradeData_GBP, historicTradeData_EUR, historicTradeData_CNY )

ggplot( historicTradeData,
        aes( unixtime, price, size = amount, group = symbol ) ) +  
  geom_point() + 
  geom_smooth( aes( weight = amount, group = symbol, color = symbol ) ) + 
  labs( x = "Time", y = "Price", color = "Symbol", size = "Volume" ) +
  guides( size = guide_legend( override.aes = list( linetype = 0 ) ) ) +
  ggtitle( "Bitcoin value over time, and relative to different currencies" )



# What happened in the last hour?
nb_hours <- 1
last_hour_data <- historicTradeData[ unixtime > ( Sys.time() - nb_hours * 3600 ), ]

ggplot( last_hour_data,
        aes( unixtime, price, size = amount, group = symbol ) ) +  
  geom_point() + 
  geom_smooth( aes( weight = amount, group = symbol, color = symbol ) ) + 
  labs( x = "Time", y = "Price", color = "Symbol", size = "Volume" ) +
  guides( size = guide_legend( override.aes = list( linetype = 0 ) ) ) +
  ggtitle( "And now this happened... " )





# Time series -------------------------------------------------------------


# Convert this data to a more typical time series format:

ChosenTimeUnit <- "hour" # could also be "min", for instance

historicTradeData_GBP[ , ChosenTimeUnit := cut( unixtime, breaks = ChosenTimeUnit ) ]
historicTradeData_GBP[, WeightedAvePricesPerTimeUnit := weighted.mean( price, amount ), by = ChosenTimeUnit ]

# Now to extract the new, equally-spaced sampling intervals.
simplified_data <- historicTradeData_GBP[ , .SD, .SDcol = c( "ChosenTimeUnit", "WeightedAvePricesPerTimeUnit" ) ]
simplified_data <- simplified_data[ ! duplicated( simplified_data ), ]
  

simplified_data[ , Date := sapply( str_split( as.character( ChosenTimeUnit ), " " ), "[[", 1 ) ]
simplified_data[ , ChosenTimeUnitWithinDay := sapply( str_split( as.character( ChosenTimeUnit ), " " ), "[[", 2 ) ]
simplified_data[ , ChosenTimeUnit := NULL ]

simplified_data_wide <- dcast( simplified_data, Date ~ ChosenTimeUnitWithinDay, value.var = "WeightedAvePricesPerTimeUnit" )

bitcoin_ts <- xts( as.vector( simplified_data_wide[ , -1 ] ), 
                   order.by = as.Date( simplified_data_wide$Date ), 
                   frequency = 24 )

# With time, once we start accumulating more data, the width of this dataset will stay fixed, but it will get longer as we add in more days.


# Example:

data(AirPassengers)
AirPassengers

str(AirPassengers)

# Conceptually, this AirPassengers data looks the same to me as the bitcoin_ts data. And yet... the bitcoin_ts is seen as multivariate, whereas AirPassengers is seen as univariate. Been trying to convert the bitcoin data to univariate and have been unsuccessfuk... Hence, functions like adf.test() below do not work (only accepting univar stuff)...


# class(AirPassengers)
# start(AirPassengers)
# frequency(AirPassengers)
# summary(AirPassengers)
# plot(AirPassengers)
# abline(reg = lm( AirPassengers ~ time( AirPassengers ) ) )
# cycle(AirPassengers)
# plot(aggregate(AirPassengers,FUN=mean))
# boxplot(AirPassengers~cycle(AirPassengers))


# Is this time series stationary or non-stationary?
# If non-stationary, cannot use time series modelling on the data.
# Use Augmented Dickey-Fuller Test (adf test). A p-Value of less than 0.05 in adf.test() indicates that it is stationary.

adf.test( bitcoin_ts ) # p-value < 0.05 indicates the TS is stationary
kpss.test( bitcoin_ts )



# Since data is stationary (as suggested by the plots previously), we can now find out what lag is most suitable, using an ACF (auto correlation function).
# From: http://r-statistics.co/Time-Series-Analysis-With-R.html :
# Autocorrelation is the correlation of a Time Series with lags of itself. This is a significant metric because it is used commonly to determine if the time series is stationary or not. A stationary time series will have the autocorrelation fall to zero fairly quickly but for a non-stationary series it drops gradually.

acf( simplified_data$WeightedMeansPerMinute )
acf( simplified_data$WeightedMeansPerMinute, type = "covariance" )
acf( simplified_data$WeightedMeansPerMinute, type = "correlation" )
acf( simplified_data$WeightedMeansPerMinute, type = "partial" )

pacf( simplified_data$WeightedMeansPerMinute )







# Network analysis --------------------------------------------------------

# Ultimately irrelevant. But found example and decided to try it out.

# Source: http://beautifuldata.net/2015/01/querying-the-bitcoin-blockchain-with-r/
# Or: https://www.r-bloggers.com/querying-the-bitcoin-blockchain-with-r/

wallet <- blockchain.api.process('15Mb2QcgF3XDMeVn6M7oCG6CQLw4mkedDi')
seed <- '1NfRMkhm5vjizzqkp2Qb28N7geRQCa4XqC'
genesis <- '1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa'
singleaddress <- blockchain.api.query( method = 'Single Address', bitcoin_address = seed, limit = 100 )
txs <- singleaddress$txs

bc <- data.frame()
for ( t in txs ) {
  hash <- t$hash
  for (inputs in t$inputs) {
    from <- inputs$prev_out$addr
    for (out in t$out) {
      to <- out$addr
      va <- out$value
      bc <- rbind( bc, 
                   data.frame( from = from,
                               to = to, 
                               value = va, 
                               stringsAsFactors = F ) )
    }
  }
}


btc <- ddply( bc, c( "from", "to" ), summarize, value = sum( value ) )


btc.net <- graph.data.frame( btc, directed = T )

V( btc.net )$color <- "blue"

V( btc.net )$color[ unlist( V( btc.net )$name ) == seed ] <- "red"

nodes <- unlist( V( btc.net )$name )

E( btc.net )$width <- log( E( btc.net )$value ) / 10

plot.igraph( btc.net, vertex.size = 5, edge.arrow.size = 0.1, 
             vertex.label = NA, 
             main = paste( "BTC transaction network for\n", seed ) )










