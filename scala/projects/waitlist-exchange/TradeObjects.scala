package tradingCircles

class OfferedTrade[A](val offer: A, var desiredPossessions: Set[A]){
    def desired(item: A): Boolean = {
        desiredPossessions(item)
    }
    def canTrade(other: OfferedTrade[A]): Boolean = {
        other.desiredPossessions(offer) && desiredPossessions(other.offer)
    }
}

class Trader[A](val name: String, var possessions: Set[A], var offeredTrades: Set[OfferedTrade[A]]){
    def findOffer(item: A): Option[OfferedTrade[A]] = {
        for (trade <- offeredTrades) {
            if(trade.offer == item) return Some(trade)
        }
        None
    }
    def involvedInTrade(td: Trade[A]): Boolean = {
        (name == td.t1.name) || (name == td.t2.name)
    }
}

class Market[A](var traders: Set[Trader[A]], var possTrades: Set[Trade[A]])

