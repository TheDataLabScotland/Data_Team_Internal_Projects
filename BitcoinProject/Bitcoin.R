

# Libraries and options ---------------------------------------------------

# install.packages( c( "plyr", "data.table", "lubridate", "Rbitcoin", "rbitcoinchartsapi", "igraph", "ggplot2" ) )

library(plyr)

library(data.table)
library(lubridate)

library(Rbitcoin)
library(rbitcoinchartsapi)

library(igraph)
library(ggplot2)


# # Choose here:
# options( scipen = 999 )
# options( scipen = 0 )




# Old stuff with downloaded data -------------------------------------------

# Source: https://www.coinigy.com/bitcoin-data/

# bitcoin_dat <- fread( "/home/caterina/Documents/TDL_Internal_Projects/Bitcoin/RAW_OK_BTCCNY_20160401_20160404.csv" )
# bitcoin_dat <- bitcoin_dat[ 1 : 10000, ]
# setnames( bitcoin_dat, c( "id", "exchange", "market", "tradeid", "price", "quantity", "total", "time_local", "type" ) )
# 
# summary( bitcoin_dat )
# 
# # bitcoin_dat[ , id := as.character( id ) ]
# bitcoin_dat[ , exchange := as.character( exchange ) ]
# bitcoin_dat[ , market := as.character( market ) ]
# bitcoin_dat[ , type := as.factor( type ) ]
# bitcoin_dat[ , time_local := lubridate::ymd_hms( time_local ) ]
# 
# head(bitcoin_dat)




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


Rbitcoin.plot( trades, col = 'blue' )
Rbitcoin.plot( order_book )



ggplot( trades$trades, aes( date, price, size = amount ) ) + 
  geom_point() + 
  geom_smooth( aes( weight = amount ) )




# These lead to errors, despite being taken from the package examples:
# See ?market.api.process
# action = "wallet"
# action = "open_orders"
# action = "cancel_orders" 
# action = "place_limit_order"
# Argument = 'raw.query.res' : logical skip post-processing are return results only after fromJSON processing. Useful in case of change results structure from market API. It can always be manually post-processed as a workaround till the Rbitcoin update.





# Package rbitcoinchartsapi -----------------------------------------------



# Source: https://coherentlogic.com/wordpress/middleware-development/r-package-for-the-bitcoincharts-com-api/

weightedPrices <- rbitcoinchartsapi::GetWeightedPrices()
weightedPrices$GBP

gbp <- rbitcoinchartsapi::GetMarketData( list( currency = "GBP" ) )
gbp[[1]]$latest_trade

historicTradeData_GBP <- rbitcoinchartsapi::GetHistoricTradeData( list( symbol = "localbtcGBP" ) )
historicTradeData_USD <- rbitcoinchartsapi::GetHistoricTradeData( list( symbol = "localbtcUSD" ) )

setDT( historicTradeData_GBP )[ , symbol := "GBP" ]
setDT( historicTradeData_USD )[ , symbol := "USD" ]

historicTradeData_GBP[  , unixtime := as.POSIXct( unixtime, origin = "1970-01-01" ) ]
historicTradeData_USD[  , unixtime := as.POSIXct( unixtime, origin = "1970-01-01" ) ]

historicTradeData <- rbind( historicTradeData_GBP, historicTradeData_USD )

ggplot( historicTradeData, aes( unixtime, price, size = amount, group = symbol ) ) + 
  geom_point() + 
  geom_smooth( aes( weight = amount, color = symbol ) )




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










