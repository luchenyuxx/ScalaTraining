
/**
  *
  * forEach  function looks like map. It takes as argument a function
  * and a list of element. But, instead of returning a list of result,
  * forEach apply the function to each element from left to right.
  * The values obtained applying the function are not used at all.
  * forEach is then used with functions which do actions, like print to the screen.
  *
  * For example
  * (for-each (lambda (x) (newline) (display x))
  * (list 57 321 88))
  * 57
  * 321
  * 88
  *
  * forEach can return a arbitrary value, like #t. Give an implementation of forEach
  *
  */

def forEach[A](function:Function[A, _], list:List[A]):Any = {
  list map (x => function(x))
}

forEach((x => print(x)), List("B","C","D"))