

object Main {
    val a: Option[Int] = Some(1)
    val b: Option[Int] = None

    for { x <- a
          //y <- b 
          } yield {println(s"x is $x")}
    println("Now the other one.")

    for ( x <- b ) yield {println(s"x is $x")}

}














