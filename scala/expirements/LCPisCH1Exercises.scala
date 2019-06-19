



// Problem 1


def compose[A, B, C](g: B => C, f: A => B): A => C = { 
    (x: A) => g(f(x))
}

// prob 2
def fuse[A, B](a: Option[A], b: Option[B]): Option[(A,B)] = { // the other definition is stupid
    a match {
        case Some(x) => b match {
                        case Some(y) => Some((x,y))
                        case _       => None
                        }
        case _       => None
    }
    
}


// prob 3
def check[T] (xs: Seq[T])(pred: T => Boolean): Boolean = {
    xs match {
        case head +: tail => if (pred(head)){
                                true
                            } else{
                                check(tail)(pred)
                            }
        case _             => false
    }
}


//Prob 4

case class pair[A, B](a: A, b: B){}


abstract class Maybe
case class Nothing() extends Maybe
case class Just[T](t: T) extends Maybe

def there(x: Maybe): Boolean = {
    x match {  //when doing this in interpreter, need to specify value of
                // thing being matched as of type Maybe
        case Nothing() => false
        case Just(value) => true
    }
}



// the permutation problem is a bit of a pain...















