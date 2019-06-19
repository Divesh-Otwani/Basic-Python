package tradingCircles
import scala.collection.mutable.Queue

/*
List of assumptions and simplifications of the model.

1. All traders have unique names
2. Traders do not change their prefernces or any other info
3. Traders do not leave markets
4. Traders do not want 3+ person trades
5. Goods have a toString method.
6. The server's response is just terminal output.5
7. Requests have the inbuilt response. We have no handles --or cases that respond
to requests by type (I'll do all that when and if I make a legit web server for all this)

Sources used:

1. Our textbook
2. Official scala documentation.
3. This http://alvinalexander.com/scala/how-to-use-queue-class-in-scala-enqueue-dequeue
4. SO: 
http://stackoverflow.com/questions/5051574/how-to-choose-a-random-element-from-an-array-in-scala

*/
object Simulation extends App {
    println("\n\nStarting the simulaton\n\n")
    val svr = new Server(Queue():Queue[Request])
    svr.start() // server is running
    // we do two things, make requests and then just send them,
    // perhaps with some timing issues, to the server

    // 1. make some server requests
    
        // preliminaries: make traders, make a market

    val classes = List("CS356", "CS240", "CS380", "Detention", "CS350", "Math")
        // for now, we just work with these classes
    var market = new Market(Set(): Set[Trader[String]], Set():Set[Trade[String]])
        // we just have one market, for now
            // this is the shared data. 
    var t11 = new OfferedTrade(classes(0), Set(classes(1), classes(2), classes(4)))
    var t12 = new OfferedTrade(classes(3), Set(classes(1), classes(2), classes(4)))
    var t21 = new OfferedTrade(classes(1), Set(classes(3)))
    var t1  = new Trader("Bob", Set(classes(0), classes(3)), Set(t11, t12))
    var t2  = new Trader("Jack", Set(classes(1)), Set(t21))
    
    
        // with preliminaties, make requests
    val request1 = new addTrader("Adding t1", t1, market)
    val request2 = new addTrader("Adding t2", t2, market)
    val request3 = new FindMyTrades("t1's Trades", t1, market)
    val request4 = new FindMyTrades("t2's Trades", t2, market)
    val request5 = new DoRandTrade("t1's 'random' --but not really-- trade", t1, market) 
    val request6 = new FindMyTrades("t1's trades now, ", t1, market)

    // 2. add a bunch of requests --perhaps with some sleeps

    svr.addRequest(request1)
    Thread.sleep(500) // If I remove this, we have an error. YAY! 
    svr.addRequest(request2)
    Thread.sleep(500)
    svr.addRequest(request3)
    Thread.sleep(500)
    svr.addRequest(request4)
    Thread.sleep(500)
    svr.addRequest(request5)
    Thread.sleep(500)
    svr.addRequest(request6)

    Thread.sleep(6500)
    svr.turnOff()
    svr.join()
    println("\n\nSimulation over. How was it? \n\n")
}



















