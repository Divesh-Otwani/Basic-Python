



class Foo()
class Bar()

object Main extends App {
  val a: Foo = new Foo()
  val b: Bar = new Bar()
  val c: Bar = a.asInstanceOf[Bar] // errors

}







