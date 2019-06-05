package tradingCircles

// Question: how can I make it so that only a speific server request can do a trade?
class Trade[A](var t1: Trader[A], var t2: Trader[A], val item1: A, val item2: A) extends Thread { 
    override def toString(): String = {
        t1.name + ", " + item1.toString + " <-> " + t2.name + ", " + item2.toString 
    }
    def isSatisfied(t: Trader[A], item_t: A, item_other: A): Boolean = {
        if (!t.possessions(item_t)) return false
        val trade = t.findOffer(item_t)
        trade match { 
            case None => return false
            case Some(offTd) => if (!offTd.desired(item_other)) return false
        }
        return true
    }
   
    def HalfTrade(t: Trader[A], item_t: A, item_other: A): Boolean = { 
        val name = t.name
        t.possessions -= item_t
        t.possessions += item_other
        val currOffer = t.findOffer(item_t)
        currOffer match {
            case None          => println("Foobar (F'ed up beyond all recognition)")
                                  println(s"Trader $name is trading something they don't want to. ")
                                  return false
            case Some(currOff) => t.offeredTrades -= currOff
        }
        for (offer <- t.offeredTrades){
            offer.desiredPossessions -= item_other
        }
        true
    }

    def compatible(): Boolean = {
        // a trade is compatible iff 
            // each party has the needed item to trade
            // each party wants to trade their item
            // the other party's offer is something they are willing to trade for
        isSatisfied(t1, item1, item2) && isSatisfied(t2, item2, item1)
    }
    
    
    override def run() = {
        val t2name = t2.name
        val t1name = t1.name
        println(s"Starting trade: $t1name , $item1 -> $t2name , $item2")
        val comp = compatible()
        if (comp){
            val part1 = HalfTrade(t1, item1, item2)
            if (part1){
                println(s"First half success: $t1name , $item1 -> $t2name , $item2")
            }else{
                println(s"First half FAILED: $t1name , $item1 -> $t2name , $item2")
            }
            val part2 = HalfTrade(t2, item2, item1)
            if(part2){ 
                println(s"Second half success: $t2name , $item2 -> $t1name , $item1 \n")
            }else{
                println(s"Second half FAILED: $t2name , $item2 -> $t1name , $item1")
            }
        }else{
            println("ERROR: Trade isn't compatiable anymore --something happened.")
            println(s"Trade of $t1name , $item1 -> $t2name , $item2 not compatible")
        }
    }   
}







