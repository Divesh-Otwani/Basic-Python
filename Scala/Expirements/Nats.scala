

object main extends App{
    println("Hi world!")
}

abstract class Nat

case object Zero extends Nat

case class Succ(x:Nat) extends Nat


def add(x: Nat, y: Nat): Nat = {
  x match {
      case Zero()  => y
      case Succ(n) => Succ(add(n, y))
  }
}


def toNat(x: Int): Nat = {
  if (x == 0) Zero()
  else Succ(toNat(x-1))
}



def fromNat(x: Nat): Int = {
  x match {
    case Zero()  => 0
    case Succ(n) => 1 + fromNat(n)
  }
}










