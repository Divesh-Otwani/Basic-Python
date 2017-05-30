

The next step is to add variable bindings 
and allow recursive definitions -- with lazyness.
 -- right now, it's not working properly ...


error:

divesh:UntypedLambdaCalc$ ./LCalc 
Welcome to Divesh's Untyped Lambda Calculus.
Here is the interface:

A Term is one of
  Iden String
  String :>> Term -- lambda
  Term :@ Term -- application
  Def String -- user made bindings
String := Term -- is how you bind
-- Note, you need all the parenthesis :(

> "x" :>> Iden "x"
"x" :>> Iden "x"
> "id" := "x" :>> Iden "x"
> (Def "id") :@ Iden "hello"
"x" :>> Iden "x"
> ^C

