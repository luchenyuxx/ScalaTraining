
/**
  *
  * We can represent a set as a collection of distinct elements
  * and we can represent the set of all subsets as a list of list.
  * For example, if the set is (1 2 3), so the set of all subsets
  * is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)).
  * Complete then the difinition of the following function which generates
  * the set of all subsets and give a clear explanation of its mechanism
  *
  *
  * (define (sous-ensemble s)
  * (if (null? s)
  * (list nil)
  * (let ((rest (sous-ensemble (cdr s))))
  * (append rest (map <??> rest)))))
  */

def subSet[A](acc:List[List[A]], list:List[A]):List[List[A]] = {
  list match {
    case Nil => acc
    case (x::tail) => subSet((x::acc.head)::(List(x))::acc, tail)
  }
}

print(subSet(List(List()), List(1,2,3)))