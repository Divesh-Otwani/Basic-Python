# Documentation

## Overall

I organize my work into 4 files.

1. Simulation.scala contains the actual simulation

2. Trade.scala contains the class extended for doing a thread

3. Server.scala contains the server class and all the Requests
    -- all of which extend thread

4. TradeObjects contain the objects that are conventional objects in a trading env


## Concrete Objects

* A market consists of a set of traders and a set of possible trades
 * A trader has a name, a set of possessions and a set of offeredTrades
    * An offered trade is simply an offered item and a set of items one is willing to trade the offered item for
 * A trade is between two traders and an item given up by each trader

### Nice Operations

These operations just turned out to be useful.

* An offered trade should be able to
 * Check if some item is desired
 * Check if that offered trade is compatible with another
* A trader should be able to
 * find, given an item, if he has an offered trade giving up that item
 * Determine, given a trade, whether he or she is a party in that trade
* A trade should (in addition to just executing that trade)
 * determine if a given trade is valid.

## Abstract Objects

* A server is a queue of requests
 * Requests are named operations (typically user given) that the server responds to. We have three. 
  * Given a trader and a market, we can add that trader to the market (and update the market's possible trades)
  * Given a trader and a market, we can see what trades in the market can happen involving that trader
  * Given a trader and a market, we can simulate a trade by having the trader make a random trade (and then remove that from the market)


## Overall Design 

I won't mention algorithms. The code is clean enough to just look at it.
Come to think of it, the Simulations.scala code is well commented. I refer you to there. 



