
# Libraries and options ---------------------------------------------------

# install.packages( c( "plyr", "data.table", "lubridate", "stringr, "tidyr", "Rbitcoin", "rbitcoinchartsapi", "igraph", "ggplot2", "forecast", "tseries", "xts", "TTR" ) )

library(plyr)

library(data.table)
library(lubridate)
library(stringr)
library(tidyr)

library(Rbitcoin)
library(rbitcoinchartsapi)

library(igraph)
library(ggplot2)

library(xts)
library(tseries)
library(forecast)
library(TTR)

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

# Figure out the oldest day we can still grab *complete* data from - otherwise, the oldest data will be *precisely* from 5 days ago, i.e., data will not be included from midnight onwards, but rather, from some other random time in the morning (during working hours, anyhow). 

oldest_complete_date <- as.Date( Sys.time( ) ) - 4


historicTradeData_GBP <- rbitcoinchartsapi::GetHistoricTradeData( list( symbol = "localbtcGBP", 
                                                                        start = as.numeric( as.POSIXct( oldest_complete_date, 
                                                                                                        format="%Y-%m-%d" ) ) ) )

historicTradeData_EUR <- rbitcoinchartsapi::GetHistoricTradeData( list( symbol = "localbtcEUR", 
                                                                        start = as.numeric( as.POSIXct( oldest_complete_date, 
                                                                                                        format="%Y-%m-%d" ) ) ) )

historicTradeData_CNY <- rbitcoinchartsapi::GetHistoricTradeData( list( symbol = "btctradeCNY",
                                                                        start = as.numeric( as.POSIXct( oldest_complete_date, 
                                                                                                        format="%Y-%m-%d" ) ) ) )

# Remember to cut off any data from today - since, as the day is not over yet, this will also be incomplete

setDT( historicTradeData_GBP )[ , symbol := "GBP" ]
setDT( historicTradeData_EUR )[ , symbol := "EUR" ]
setDT( historicTradeData_CNY )[ , symbol := "CNY" ]

historicTradeData_GBP[  , unixtime := as.POSIXct( unixtime, origin = "1970-01-01" ) ]
historicTradeData_GBP <- historicTradeData_GBP[ unixtime < as.Date( Sys.time() ), ]

historicTradeData_EUR[  , unixtime := as.POSIXct( unixtime, origin = "1970-01-01" ) ]
historicTradeData_EUR <- historicTradeData_EUR[ unixtime < as.Date( Sys.time() ), ]

historicTradeData_CNY[  , unixtime := as.POSIXct( unixtime, origin = "1970-01-01" ) ]
historicTradeData_CNY <- historicTradeData_CNY[ unixtime < as.Date( Sys.time() ), ]

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





# Grabbing daily Bitcoin data ---------------------------------------------

# Alternative to the previous code - once enough data is gathered (maybe 1 month's worth?), can switch entirely to this data:

RData_bitcoin_files <- list.files( "/home/caterina/Documents/Data_Team_Internal_Projects/BitcoinProject", 
                                   pattern = "*.RData$", full.names = TRUE )

RData_bitcoin_files_short <- list.files( "/home/caterina/Documents/Data_Team_Internal_Projects/BitcoinProject", 
                                         pattern = "*.RData$", full.names = FALSE )


EUR_transactions <- list()
GBP_transactions <- list()
for ( RData_file in RData_bitcoin_files ) {
  print( RData_file )
  EUR_and_GBP_to_BTN <- get( load( RData_file ) )
  EUR_transactions[[ which( RData_bitcoin_files == RData_file ) ]] <- historicTradeData_EUR_yesterday
  GBP_transactions[[ which( RData_bitcoin_files == RData_file ) ]] <- historicTradeData_GBP_yesterday
}

# Clean up workspace by removing the objects containing just the most recent day of data collection:
rm( list = c( "historicTradeData_GBP_yesterday", "historicTradeData_EUR_yesterday" ) )

# Also create full-size objects, containing in each case, the full data collected across the days of measurement.
historicTradeData_EUR <- rbindlist( EUR_transactions )
historicTradeData_GBP <- rbindlist( GBP_transactions )

historicTradeData_GBP[ , unixtime2 := as.POSIXct( unixtime, origin = "1970-01-01", tz = "UTC" ) ]
historicTradeData_EUR[ , unixtime2 := as.POSIXct( unixtime, origin = "1970-01-01", tz = "UTC" ) ]

# # It looks like daylight saving was applied in the script that was pulling down the data (pre 30 Oct 2017)... which confuses things later.
# Fixing it here:

# With hackery:
# historicTradeData_GBP <- historicTradeData_GBP[ unixtime > "2017-10-30" , ]
# historicTradeData_EUR <- historicTradeData_EUR[ unixtime > "2017-10-30" , ]

# By trying to go back to the original unixtime, then convert back to POSIXct, although this time using GMT as tz, instead of the local tz which is used by default... So this removes daylight saving adjustments:
historicTradeData_GBP[ , unixtime := as.POSIXct( as.numeric( unixtime ),  origin = "1970-01-01", tz = "GMT") ]
historicTradeData_EUR[ , unixtime := as.POSIXct( as.numeric( unixtime ),  origin = "1970-01-01", tz = "GMT") ]

# However, this still does not get around the issue of there being one missing hour in the data, before 30 Oct (Why?!?!?!)

# Time series ------------------------------------------------------------

# xts version -------------------------------------------------------------

# (optional)

bitcoin_xts <- xts( historicTradeData_GBP$price,
                    order.by = as.POSIXct( historicTradeData_GBP$unixtime ) )
coredata( bitcoin_xts )
bitcoin_hourly_xts <- to.period( bitcoin_xts, period = 'hours' )

plot( bitcoin_xts )



# ts version --------------------------------------------------------

# The more involved option, with the possibility of one value per cell, which is a weighted mean:
# Convert this data to a more typical time series format:
ChosenTimeUnit <- "hour" # could also be "min", for instance

historicTradeData_GBP[ , ChosenTimeUnit := cut( unixtime, breaks = ChosenTimeUnit ) ]
# Looks like there are no trades recorded between midnight/00.00 and 1.00am, hence there are 23 measures per day, instead of 24.
table( lubridate::hour( historicTradeData_GBP$unixtime ) )

historicTradeData_GBP[ , WeightedAvePricesPerTimeUnit := weighted.mean( price, amount ), by = ChosenTimeUnit ]

# Now to extract the new, equally-spaced sampling intervals.
simplified_data <- historicTradeData_GBP[ , .SD, .SDcol = c( "ChosenTimeUnit", 
                                                             "WeightedAvePricesPerTimeUnit" ) ]
simplified_data <- simplified_data[ ! duplicated( simplified_data ), ]
  

simplified_data[ , Date := sapply( str_split( as.character( ChosenTimeUnit ), " " ), "[[", 1 ) ]
simplified_data[ , ChosenTimeUnitWithinDay := sapply( str_split( as.character( ChosenTimeUnit ), " " ), "[[", 2 ) ]
simplified_data[ , ChosenTimeUnit := NULL ]


# This adds in NA values whenever a certain hour has no data. Otherwise, this does not get flagged into the long-format data.
simplified_data_wide <- dcast( simplified_data, 
                               Date ~ ChosenTimeUnitWithinDay, 
                               value.var = "WeightedAvePricesPerTimeUnit" )

# Impute values from previous cell, whenever there is missing data. Since fill() fills empty cells vertically, going to temporarily transpose this data:
simplified_data_wide_t <- data.table( t( simplified_data_wide[ , - 1] ) )
simplified_data_wide_t <- data.table( apply( simplified_data_wide_t, 2, as.numeric ) )

# Fill any missing values / cells with the most recent price recorded:
simplified_data_wide_t <- fill( simplified_data_wide_t, names( simplified_data_wide_t ), .direction = "down" )

readd_dates <- simplified_data_wide$Date
column_names <- names( simplified_data_wide )
simplified_data_wide <- t( simplified_data_wide_t )
simplified_data_wide <- data.table( readd_dates, simplified_data_wide )
setnames( simplified_data_wide, column_names )


# Back to wide - but this time with the missing slots flagged up. This is useful in order to have a constant frequency / number of columns or values per day (i.e., 24 hours per day).
simplified_data_long <- melt( simplified_data_wide, 
                              id.vars = 'Date', 
                              value.name = "WeightedAvePricesPerTimeUnit", 
                              variable.name = "Hour" )
simplified_data_long <- simplified_data_long[ order( Date, Hour ), ]
 
bitcoin_ts <- ts( simplified_data_long$WeightedAvePricesPerTimeUnit, 
                  frequency = 23 )

cycle( bitcoin_ts )
summary( bitcoin_ts )

plot( bitcoin_ts )
plot( aggregate( bitcoin_ts, FUN = mean ) ) # Average price for each day of measurement.
boxplot( bitcoin_ts ~ cycle( bitcoin_ts ) ) # Shows how things vary across the time of day (by considering data from all days within each boxplot)



# With time, once we start accumulating more data, the width of this dataset will stay fixed, but it will get longer as we add in more days.



# Is this time series stationary or non-stationary?
# If non-stationary, cannot use time series modelling on the data.
# Use Augmented Dickey-Fuller Test (adf test). A p-Value of less than 0.05 in adf.test() indicates that it is stationary.


adf.test( bitcoin_ts ) # p-value < 0.05 indicates the TS is stationary
kpss.test( bitcoin_ts )

# OR:
forecast::ndiffs( bitcoin_ts, alpha = 0.05, test = "kpss", max.d = 2 )

# Since data is stationary (as suggested by the plots previously), we can now find out what lag is most suitable, using an ACF (auto correlation function). Otherwise (P)ACF would be meaningless.
# From: http://r-statistics.co/Time-Series-Analysis-With-R.html :
# Autocorrelation is the correlation of a Time Series with lags of itself. This is a significant metric because it is used commonly to determine if the time series is stationary or not. A stationary time series will have the autocorrelation fall to zero fairly quickly but for a non-stationary series it drops gradually.

# From: https://www.youtube.com/watch?v=tJ-O3hk1vRw (Jeffrey Yau | Applied Time Series Econometrics in Python and R)
# Pattern of ACF and PACF associated with AR, MA and ARMA processes:

# This applies to, and needs to be specified for, any SEASONAL and NON-SEASONAL components alike:
# Process   
#      |         ACF                PACF
# AR   | tails off          | cutoff after lag p
# MA   | cutoff after lag q | tails off
# ARMA | tails off          | tails off



# acf( na.omit( bitcoin_ts ) , type = "covariance" )
 

# Smoothing the time series with a moving average, to view the 'trend component' in the data:
# From: https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
plot( bitcoin_ts )
bitcoin_ts_sma <- ts( SMA( bitcoin_ts, n = 24 ), frequency = 23 )
plot( bitcoin_ts_sma )


# "A seasonal time series consists of a trend component, a seasonal component and an irregular component. Decomposing the time series means separating the time series into these three components: that is, estimating these three components."
decomposed_bitcoin_ts <- stats::decompose( bitcoin_ts )
plot( decomposed_bitcoin_ts )

# Using these decomposed streams, *I think* we can take a look at the overall trend in prices across the days tested - regardless of the hourly variations:
# I think in our case, 'seasonal' variation corresponds to 'daily' variations in prices.
bitcoin_ts_seasonally_adjusted <- bitcoin_ts - decomposed_bitcoin_ts$seasonal
plot( bitcoin_ts_seasonally_adjusted )
# vs
plot( bitcoin_ts )

# Similar idea:
bitcoin_ts_seasonally_and_randomness_adjusted <- bitcoin_ts - decomposed_bitcoin_ts$seasonal - decomposed_bitcoin_ts$random
plot( bitcoin_ts_seasonally_and_randomness_adjusted ) # Very similar to the SMA ts.




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










