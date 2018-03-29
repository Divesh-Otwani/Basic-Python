/*

In this file I run thorough examples of most of the features of Scala.
It is a reference for the basic to medium features of the language.

*/

object CrashCourse extends App{
  println("Hello, Scala!")
  //var calc = new Calculator("name")
  for (arg <- args) println(arg)
}
// We can see the impact of the above
//divesh:CrashCourse$ scala CrashCourse this is a test
//Hello, Scala!
//this
//is
//a
//test


// traits are tricky, they are like data fields or
    // mere function typed out values that promise to be implemented
    // they are the most abstract idea of an interface -- functions

trait Identify {
    def announce: Unit;
    val brand: String;
}

//Class example
class Calculator(name: String) extends Identify { //could extend

    val brand: String = "TI83"
    def announce: Unit = {
        println(name + " is here!");
    }
    def add(x: Int, y: Int) = { x + y } // function return type inferred


}

// below is a singleton object, the first one is one too,
// it acts like a main getting s Seq(String) -- a sequence of strings

object Learning {
    def printAll(args: String*) = { //example of argument lists, there is only one and it comes last
      args.map {arg => println(arg)}
    }
  val val1 = 2; // val can't be changed
  var var1 = 3;
  var name = 3;
  var1 += 1; // var can be changed


    val anonyFunc = (i: Int) => i + 1

    def add(x: Int, y: Int) : Int = {
        x + y
    }
    val addOne = add(1, _:Int); // Curry functions at any arguement(s)

    val newcalc = new Calculator("Bob the calculator");
    newcalc.announce; // In order to see this, you need to RUN
                        // the Learning object by in the interpreter, typeing
                        // > Learning

    //type parameter syntax --paramentric and, observe that it works on classes too
    def id[A](a: A): A = {a} // this MUST be the case


}

























