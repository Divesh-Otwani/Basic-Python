package tradingCircles
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer
import java.util.Random

// Note: this allows for multiple servers. I won't use them and should probably have used
// a singleton, but meh.
class Server(var requestQ: Queue[Request], var stillRunning: Boolean = true) extends Thread {
    override def run(){
        while(stillRunning){
            val requests = requestQ.dequeueAll((x: Request) => true)
            for(rq <- requests){
                val nm = rq.name
                println(s"\nStarting request $nm ...")
                rq.start()
                // rq.join() // for debugging, 
                // println(s"Request $nm is done.\n")
            }
            Thread.sleep(100) // a reasonably stupid pause
        }
    }
    def addRequest(r: Request){
        requestQ += r // this should append
    }
    def turnOff(){
        stillRunning = false
    }
}

trait Name {
    val name: String
}

abstract class Request extends Thread with Name// requests should be named

// We have, for now, three types of requests
    // 1. Adding traders to a market
    // 2. A trader asking for all possible trades they can do in a given market
    // 3. A trader asking to randomly perform one of their trades, if any, in a given market

// In the future, I'll add: removing traders, traders updating their info, cycles instead of trades
    // 

class addTrader[A](val name: String, val t: Trader[A], var m: Market[A]) extends Request {
    override def run() = {
        println("Traders before change: ---------------")
        for(trader <- m.traders){
            val nm = trader.name
            println(s"trader: $nm")
        }
        println("--------------------------------------")
        m.traders += t
        println("Traders after change: ----------------")
        for(trader <- m.traders){
            val nm = trader.name
            println(s"trader: $nm")
        }
        println("--------------------------------------")
    
        // Note, in general, we want to not allow traders to trade with themselves, 
        // but this is not enforced. This could impact the stuff below
        
        // for each of this trader's offeredtrades
            // loop through all the possible offered trades in the entire market, 
            // find which ones are compatible with this one, 
                //and then add that as a trade to the market
        for (currOffTd <- t.offeredTrades){
            for(trader <- m.traders){
                for(offTd <- trader.offeredTrades){
                    if (offTd.canTrade(currOffTd)){
                        m.possTrades += new Trade(t, trader, currOffTd.offer, offTd.offer)
                    }
                }
            }
        }
    }
}

class FindMyTrades[A](val name: String, val t: Trader[A],  val m: Market[A]) extends Request {
    override def run(){
        var anyfound = false
        for (td <- m.possTrades){
            if (t.involvedInTrade(td)){
                anyfound = true
                val tname = t.name
                val tdstr = td.toString
                println(s"Poss trade for $tname: \n\t$tdstr ")
            }
        }
        if (!anyfound){ 
            println("No trades found. \n")
        }
    }
}

class DoRandTrade[A](val name: String, var t: Trader[A], var m: Market[A]) extends Request {
    override def run(){
        var possTrades = ArrayBuffer(): ArrayBuffer[Trade[A]]
        for (td <- m.possTrades){
            if (t.involvedInTrade(td)){
                possTrades += td
            }
        }
        // next three lines courtesy of the S.O. , I did basically take them from there
            // but I understand them fully.
        val rand = new Random(System.currentTimeMillis())
        val randomIndex = rand.nextInt(possTrades.length)
        val chosenTd = possTrades(randomIndex)
        chosenTd.start()
        m.possTrades -= chosenTd
    }
}







