/**
  * Define a reverse function which takes as argument a list and return the list
  * with the same element but in reverse order
  * (renverse (list 1 4 9 16 25))
  * (25 16 9 4 1)
  */

def reverse[A](acc: List[A], list:List[A]):List[A] = {list match {
    case Nil => acc
    case (x::tail) => reverse(x::acc,tail)
  }
}

println(reverse(List(),List(1,2,3)))

/**
  * Modify your function to produce
  * a deepReverse function which takes
  * a list as argument and returns
  * the reversed list and all its reversed
  * sublist
  * For example,
  *
  * (define x (list (list 1 2) (list 3 4)))
  * x
  * ((1 2) (3 4))
  * (reverse x)
  * ((3 4) (1 2))
  * (renverse-en-profondeur x)
  * ((4 3) (2 1))
  *
  */

def deepReverse[A](acc: List[List[A]], list:List[List[A]]):List[List[A]] = {
  list match {
    case Nil => acc
    case (x::tail) => deepReverse(reverse(List(), x)::acc,tail)
  }
}

println(deepReverse(List(List()),List(List(1, 2),List(3, 4))))
