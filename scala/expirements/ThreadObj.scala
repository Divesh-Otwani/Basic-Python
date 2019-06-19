

class Holder(t: Thread){
    def startAndWait {
        t.start()
        t.join()
    }
}


object Main extends App {
    def retTh(x : Int): Thread = {
        new Thread {
            override def run {
                println(s"x is $x")
            }
        }
    }
    val t1 = retTh(3)
    val h1 = new Holder(t1)
    h1.startAndWait

}









