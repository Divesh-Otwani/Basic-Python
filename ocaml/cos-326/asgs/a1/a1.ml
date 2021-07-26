(*** COS 326 Problem Set 1 ***)

let undefined : unit -> 'a = fun () -> failwith "undefined"

(* 1. Please define these variables with the appropriate values.
 * Be sure that these statements all type-check after editing them.
 * You can do this by compiling with "ocamlbuild" in the terminal
 * emulator, or by using an evaluation plugin installed in your editor,
 * for example, Ctrl+c and then Ctrl+e in Emacs with Tuareg mode *)

(* 1.a. Create a string with your first name *)
let name : string = undefined ()

(* 1.b. Use a string operator on the string from 1.a. to create
 * a string that contains both your first and last names. *)
let name : string = undefined ()

(* 1.c. Create a string containing your email address *)
let email : string = undefined ()

(* 1.d. Replace (Other "...") in class_year with the appropriate item below *)
(* ie: replace (Other "...") with Sophomore or Junior for example *)
type year = Freshman | Sophomore | Junior | Senior | Other of string

let class_year : year = Other "I haven't filled it in yet"

(* 1.e. Replace the .... with what you're excited about in this course *)
let exciting : string = "I'm excited about ....!"

let print = Printf.printf

let print_survey () =
  let string_year =
    (match class_year with
       | Freshman -> "2023"
       | Sophomore -> "2022"
       | Junior -> "2021"
       | Senior -> "2020"
       | Other s -> "Other: " ^ s
    ) in
    (print "----------------------------------------\n";
     print "Name: %s\n" name;
     print "Email: %s\n" email;
     print "Year: %s\n" string_year;
     print "%s\n" exciting;
     print "----------------------------------------\n\n")

(* Problem 2 - Fill in types:
 * Replace each ??? with the appropriate type of the corresponding expression.
 * Be sure to remove the comments from each subproblem and to type check it
 * before submission.
 * Note that the expressions might not do anything useful -- and in fact
 * might even display interesting problems! -- but all you should do is fill
 * in the ???s to make them type check. *)

(* Problem 2a *)
(*
let prob2a : ???  = let greet y = "Hello " ^ y in greet "World!"
*)

(* Problem 2b *)
(*
let prob2b : ??? = float_of_int( int_of_float(2.2 +. 7.7))
*)

(*>* Problem 2c *>*)
(*
let rec prob2c (x : ???) : ??? =
  prob2c ( if true then prob2c x else 'h')
*)

(*>* Problem 2d *>*)
(*
let rec prob2d (y:???) (z:???) : ??? =
   prob2d (prob2d z y) (not y)
*)

(* Explain why each of 3a, 3b, 3c will not compile (use the strings
 * exp3{a,b,c} for your answers) and change the code in some small way
   so that it does, and leave prob3{a,b,c} uncommented. Do not change
   the top-level type associated with the expression. *)

(*>* Problem 3a *>*)
let exp3a : string = ""
(*
let prob3a : bool =
  let compare x y = x < y in
  compare 3.9 4
*)

(*>* Problem 3b *>*)
let exp3b : string = ""
(*
let prob3b : int =
  let fib n =
   let rec aux n y x =
    if n <= 0 then x
    else aux n-1 x+y y
   in
   aux n 1 0
  in
  fib 10
*)


(*>* Problem 3c *>*)
let exp3c : string = ""
(*
let prob3c : int =
  let sumTo (n:int) : int =
  if n <= 0 then 0
  else n + sumTo (n-1)
  in
  sumTo 10
*)

(*>* Problem 4 *>*)
(* 4a: Fill in the ??? with an expression that uses x and y and has
 * the right type.
 *
*)

(*
let prob4a =
  let u = 32.0 in
  let v = 28.0 in
  let square w = w *. w in
  let boff (x) (y) = ??? in
  let d = sqrt (boff u v) in
  int_of_float d
*)

(*
 * Also:  What warning message do you get if your ??? expression does not
 * use the function "square"?
 *)
let warn4a : string = ""

(* 4b: Replace each ?? with the type of the corresponding expression,
 * and write a function f that has the correct type signature. Explain
 * in exp4b a problem that remains with the function prob4b *)

(*
let f (a:??) (b:??) : ?? =



let rec prob4b (x:??) (y:??) : ?? =
  prob4b (f y 4) (int_of_float x)

*)

let exp4b : string = ""

(* 4c: Is it possible to find types for the argument and result that
 * make the function forever type check?
 *
 * Either give correct types or explain why it is impossible in the
 * string exp4c *)

(*
let rec forever (x:??) : ?? =
  forever forever

let exp4c : string = ""
*)

(*>* Problem 5 *>*)

exception BadDivisors of int * int
let bad_divisors n m = raise (BadDivisors (n,m))

(* Write the function few_divisors, which takes two parameters n and m,
 * and should return true if n has fewer than m divisors,
 * (including 1 and n) and false otherwise: *)
(*
let _ = few_divisors 17 3 (* true -- 17 has only 1 and 17 *)
let _ = few_divisors 4 3  (* false -- 4 has 1, 4, and 2 *)
let _ = few_divisors 4 4  (* true -- 4 has only 1, 4, and 2 *)
let _ = few_divisors 18 6 (* false -- 18 has 1, 18, 2, 3, 6, and 9 *)
let _ = few_divisors 18 7 (* true -- 18 has only 1, 18, 2, 3, 6, and 9 *)
 *)

(* The type signature for few_divisors is:
 * few_divisors : int -> int -> bool
 *)

(* few_divisors should call the function bad_divisors n m defined above
 * if n <= 0 or m < 0
 *)

(* After writing few_divisors above, uncomment the following lines to test your
 * code.  (Note: your code is not necessarily completely correct just because
 * it passes these 3 tests.)  *)
(*

let _ = assert (few_divisors 17 3)
let _ = assert (not (few_divisors 4 3))
let _ = assert (few_divisors 4 4)

*)

(* Problem 6 - Approximating Pi *)

exception BadPi of int
let bad_pi (n:int) = raise (BadPi n)

(*>* Problem 6a - Sinusoidal Approximation *>*)

(* Use the following equations to define a function sin_pi that returns
 * the ith approximation of pi.

 * approx(0) = 3
 * approx(n+1) = approx(n) + sin(approx(n))

 * Using this approximation, you will converge on many digits of pi very
 * fast.  The first few digits of pi are 3.14159 26535 89793 23846 26433.
 * Approximation 1 accurately predicts these digits:  3.141
 * Approximation 2 accurately predicts these digits:  3.14159 26535
 * Approximation 3 accurately predicts these digits:  3.14159 26535 89793
 *
 *)

(* The type signature for sin_pi is:
 *   sin_pi : int -> float
*)

(* sin_pi should call the function bad_pi i if its argument i is less than 0 *)


(* Problem 7 - Collatz Sequence *)
(* A Collatz sequence beginning with x0 is a series of positive integers

 * x0 x1 x2 x3 x4 ...

 * such that for all i >= 0, the value of x[i+1] may be computed from the
 * value of x[i] as follows:

 * x[i+1] = x[i] / 2 if x[i] is even
 * x[i+1] = 3*x[i] + 1 if x[i] is odd

 * The Collatz Conjecture states that no matter what positive number x0 you
 * start a Collatz sequence with, the sequence will eventually reach the
 * number 1 --- ie, for any Collatz sequence, there exists an i such that
 * ith element of the sequence is 1. Whenever you reach 1, you will enter
 * an infinite cycle 4 2 1 4 2 1 4 2 1 ...
 * We say that the length of the Collatz sequence beginning with x0 is the
 * number of elements in the sequence up to and including the first
 * occurrence of 1.

 * Here are some Collatz sequences, with their lengths in parenthesis afterwards:

 * 12, 6, 3, 10, 5, 16, 8, 4, 2, 1 (10)
 * 19, 58, 29, 88, 44, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1 (21)
 * 2, 1 (2)
 * 1 (1)

 * Write a function collatz : int -> int such that:
     * collatz n = 0,    if n <= 0, or
     * collatz n = k,    if n > 0 where k is the length of the
                           Collatz sequence starting at n.

 * Note:  If the Collatz sequence beginning with some n never reaches 1,
   your function does not have to terminate on that input n.

 * Note2:  Extra credit if your function raises an exception on
   inputs that never reach 1 (and only those inputs).
   This is a joke.
   And not joke: you'd certainly get lots of extra credit
    if you could prove that you've done that correctly,
    and by the way you'd be instantly famous.  See also:
   https://www.quantamagazine.org/mathematician-terence-tao-and-the-collatz-conjecture-20191211/

 * Note 3: As a practical matter, not related to Note 2,
   if 3x+1 exceeds the integers that can be represented in 63 bits or
   whatever, OCaml may silently overflow into negative numbers,
   or wrong positive numbers.  Don't worry about getting
   correct results on inputs that lead to this kind of overflow.

*)
