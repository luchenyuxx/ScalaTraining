
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]):Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as:A*):List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail:_*))


}


/**
  * What will be the result of the following match expression
  */
val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
 // case Cons(h, t) => h + sum(t)
  case _ => 101
}

/**
  * Implement the function tail for removing
  * the first element of a list.
  * Note that the function takes constant time.
  * What are different choices you could make
  * in your implementation if the list is Nil?
  * */

/**
  * Better to return Option. tail of an empty List doesn't exist
  * */
def tail[A](list: List[A]): Option[List[A]] = list match {
  case Nil => None
  case Cons(x, xs) => Some(xs)
}

var res = tail(List(1,2,3,4,5))

/**
  * Using the same idea, implement the
  * function setHead for replacing the
  * first element of a listwith a different
  * value
  */

/**
  * Better to return Option.
  * */
def setHead[A](list:List[A], h:A): Option[List[A]] = list match {
  case Nil => None
  case Cons(head, tail) => Some(Cons(h, tail))
}

var res3 = setHead(List(1,2,3,4,5), 9)

/**
  * Generalize tail to the function drop,
  * which removes the first n elements
  * from a list. Note that this function
  * takes time proportional only to the
  * number of elements being dropped _
  * we don't need to make a copy of the
  * entire list
  */
def drop[A](l:List[A], n:Int):List[A] = {
  @annotation.tailrec
  def loop(index:Int, acc:List[A]):List[A] = {
    if (index >= n) tail(acc)
    else loop(index + 1, tail(acc))
  }

  loop(1, l)
}

var res4 = drop(List(1,2,3,4,5),4)

/**
  * Implement dropWhile, which removes elements
  * from the list prefix as long as they match
  * a predicate a predicate
  */
def dropWhile[A](l: List[A], f:A=>Boolean):List[A] = {
  @annotation.tailrec
  def loop(acc:List[A], res:List[A]):List[A] = {
      acc match {
        case Nil => res
        case Cons(head, tail) => {
          if (f(head)) loop(tail, Cons(head, res))
          else loop(tail, res)
        }
      }
  }
  loop(l, Nil)
}

var res5 = dropWhile(List(3,7,4,1,0,12,7), (x:Int) => (x>4))

def append[A](a1:List[A], a2:List[A]):List[A] = a1 match {
  case Nil => a2
  case Cons(h,tail) => Cons(h, append(tail, a2))
}

/**
  * Not everything works so nicely. Implement a function,
  * init, that returns a list consisting of all but the
  * last element of a list. So, given list(1,2,3,4),
  * init will return list(1,2,3). Why can't this
  * function be implemented in constant time like tail?
  *
  */
def init[A](l:List[A]):List[A] = {

  @annotation.tailrec
  def loop(acc:List[A], res:List[A]):List[A] = {
    acc match {
      case Nil => Nil
      case Cons(last, Nil) => res
      case Cons(head, tail) => loop(tail, append(res, List(head)))
    }
  }
  loop(l,Nil)
}

var res6 = init(List(1,2,3,4,5))

def foldRight[A,B](as: List[A], z:B)(f:(A,B) => B):B = {
  as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs,z)(f))
  }
}

/**
  * Compute the length of a list using foldRight
  */
def length[A](as:List[A]):Int = {
  foldRight(as, 0)((a:A, x:Int) => (x+1))
}

var res7 = length(List(1,2,3,4,5,6))

/**
  * Our implementation of foldRigh is not tail
  * recursive nd will result in a StackOverflow
  * Error for large list (it's not stack safe).
  * Write another general list-recursion function
  *foldLeft, that is tail-recursive, using the
  * technique we discussed in the previous chapter.
  *
  * Write sum, product, and a function to compute
  * the length of a list using foldLeft
  */
def foldLeft[A,B](as:List[A], z:B)(f:(B,A) => B):B = {
  as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
  }
}

def sum(as:List[Int]):Int = {
  foldLeft(as, 0)((x,y)=>(x+y))
}

sum(List(1,2,3,4,5))

def product(as:List[Double]):Double = {
  foldLeft(as, 1.0)((x,y)=>(x*y))
}

product(List(1,2,3,4,5))

def length2[A](as:List[A]):Int = {
  foldLeft(as, 0)((x,y) => (x+1))
}

var res8= length2((List(1,2,3,4,5)))

/**
  * Write a function that returns the reverse
  * of a list (given list(1,2,3) it returns
  * list(3,2,1)). See if you can write it using a fold
  */
def reverse[A](as:List[A]):List[A] = {
  foldLeft(as, Nil:List[A])((x:List[A],y:A) => Cons(y,x))
}

var res9 = reverse(List(1,2,3,4,5))

/**
  * Hard: Can you write foldLeft in terms of foldRight?
  * How about the other way around?
  */

def foldLeftWithRight[A,B](as:List[A], z:B)(f:(B,A) => B):B = {
  as match {
    case Nil => z
      //(as: List[A], z:B)(f:(A,B) => B):B
    case Cons(x, xs) => foldLeftWithRight(xs, foldRight(xs,f(z,x))((a:A,b:B) => b))(f)
  }
}

def reverseWithFoldLeftWithRight[A](as:List[A]):List[A] = {
  foldLeftWithRight(as, Nil:List[A])((x:List[A],y:A) => Cons(y,x))
}

var res10 = reverseWithFoldLeftWithRight(List(1,2,3,4,5))

def foldRightWithLeft[A,B](as:List[A], z:B)(f:(A,B) => B):B = {
  as match {
    case Nil => z
    case Cons(x, xs) => foldRightWithLeft(xs, foldLeft(xs,f(x,z))((b:B,a:A) => b))(f)
  }
}

def reverseWithFoldRightWithLeft[A](as:List[A]):List[A] = {
  foldRightWithLeft(as, Nil:List[A])((y:A, x:List[A]) => Cons(y,x))
}

var res11 = reverseWithFoldRightWithLeft(List(1,2,3,4,5))

/**
  * Implement append in terms of either foldLeft or foldRight
  */
def appendWithFoldRight[A](l: List[A], r: List[A]): List[A] =
  foldRight(l, r)((x:A,y:List[A]) => Cons(x,y))

var res12 = appendWithFoldRight(List(1,2,3,4,5), List(6,7))


/**
  * Hard: Write a function that concatenates a list of lists
  * into a single list. Its runtime should be linear in the total
  * length of all lists.
  */
def concatenateList[A](metaList:List[List[A]]):List[A] = {
  def g(list:List[List[A]], acc:List[A]):List[A] = {
    list match {
      case Nil => acc
      case Cons(x,xs) => g(xs, append(acc, x))
    }
  }
  g(metaList,Nil)
}

var res13 = concatenateList(List(List(1,2,3,4,5), List(6,7)))

/**
  * Write a function that transforms a list
  * of integers by adding 1 to each element.
  *
  */
def add1(input:List[Int]):List[Int] = {
  def localAdd1(list:List[Int], acc:List[Int]):List[Int] = list match {
    case Nil => acc
    case Cons(x,xs) => localAdd1(xs, Cons((x+1),acc))
  }
  localAdd1(input, Nil)
}

var res14 = add1(List(1,2,3,4,5))

/**
  * Write a function that turns each value in a List[Double]
  * into a String. You can use d.toString
  */
def doubleToString(input:List[Double]):List[String] = {
  foldRight(input, Nil:List[String])((y:Double,x:List[String]) => Cons(y.toString, x))
}

var res15 = doubleToString(List(1,2,3,4,5))

/**
  * Write a function map that generalizes modifying each element
  * in a list while maintaining the structure of the list.
  * Here is its signature
  */
def map[A,B](as:List[A])(f:A=>B):List[B] = {
  foldRight(as, Nil:List[B])((y:A,x:List[B]) => Cons(f(y), x))
}

var res16 = map(List(1,2,3,4,5))(x => x+2)

/**
  * Write a function filter that removes elements
  * from a list unless they satisfy a given predicate.
  * Use it to remove all odd numbers from a list
  */
def filter[A](as:List[A])(f:A => Boolean):List[A] = {
  foldRight(as, Nil:List[A])((y:A,x:List[A]) => {
    if(f(y)) Cons(y,x)
    else x
  })
}

var res17 = filter(List(1,2,3,4,5))(x => (x%2 == 0))

/**
  * Write a function flatMap that works like map except
  * that the function given will return a list instead
  * of a single result, and that list should be inserted
  * into the final resulting list
  *
  * flatMap(List(1,2,3))(i => List(i,i)) should result in
  * List(1,1,2,2,3,3)
  * */
def flatMap[A,B](as:List[A])(f:A=>List[B]): List[B] = {
  foldLeft(as, Nil:List[B])((b:List[B], a:A) => (append(b, f(a))))
}

var res18 = flatMap(List(1,2,3))(i => List(i,i))

/**
  * Use flatMap to implement filter
  */
def filterWithFlatMap[A](as:List[A])(f:A => Boolean):List[A] = {
  flatMap(as)(a => {
  if(f(a)) List(a)
  else Nil
  })
}

var res19 = filterWithFlatMap(List(1,2,3,4,5))(x => (x%2 == 0))

/**
  * Use flatMap to implement filter
  */
def flatMapWithFilter[A](as:List[A])(f:A=>Boolean): List[A] = {
  flatMap(as)(x => if (f(x)) List(x) else Nil)
}

/**
  * Write a function that accepts 2 lists and constructs a new
  * list by adding corresponding elements. For example, List
  * (1,2,3) and List(4,5,6) become List(5,7,9)
  * */
def addLists(l1: List[Int],l2:List[Int]):List[Int] = {
  (l1, l2) match {
    case (Nil, Nil) => Nil
    case (Cons(x1,x1s),Nil) => Cons(x1, addLists(x1s,Nil))
    case (Nil, Cons(x2,x2s)) => Cons(x2, addLists(Nil,x2s))
    case (Cons(x1,x1s),Cons(x2,x2s)) => Cons(x1+x2, addLists(x1s,x2s))
  }
}

var res20 = addLists(List(1,2,3), List(4,5,6))

/**
  * Generalize the function you just wrote so that
  * it's not specific to integers or addition.
  * Name your generalized function zipWith
  */

def zipWith[A](l1: List[A],l2:List[A])(f:(A,A) => A):List[A] = {
  (l1, l2) match {
    case (Nil, Nil) => Nil
    case (Cons(x1,x1s),Nil) => Cons(x1, zipWith(x1s,Nil)(f))
    case (Nil, Cons(x2,x2s)) => Cons(x2, zipWith(Nil,x2s)(f))
    case (Cons(x1,x1s),Cons(x2,x2s)) => Cons(f(x1,x2), zipWith(x1s,x2s)(f))
  }
}

var res21 = zipWith(List(1,2,3), List(4,5,6))((x,y) => x+y)

/**
  * Hard: As an example, implement hasSubsequence for checking
  * whether a list contains another list as a subsequence.
  * For instance, List(1,2,3,4) would have List(1,2), List(2,3)
  * and List(4) as subsequences, among others. You may have
  * some difficulty finding a concise purely functional imple
  * mentation that is also efficient. That's okay. Implement
  * the function however comes most naturally. We'll return
  * to this implementation in chapter 5 and hopefully improve
  * on it. Note: Any two values x and y can be compared for
  * equality in scala using the expression x == y
  */
def hasSubsequence[A](sup:List[A], sub:List[A]):Boolean = {
  def isEmpty(l:List[A]):Boolean = l match {
    case Nil => true
    case Cons(x,xs) => false
  }

  @annotation.tailrec
  def isEmbedded(l1:List[A], l2:List[A], acc:Boolean): Boolean = {
    l2 match {
      case Nil => true
      case Cons(x,xs)  => {
        l1 match {
          case Nil => acc
          case Cons(y,ys) => isEmbedded(xs, ys, acc && (x == y))
        }
      }
    }
  }
  isEmbedded(sup,sub, !isEmpty(sup))
}

var res22 = hasSubsequence(List(1,2,3,4), List(2,3))

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]

/**
  * Write a function size that counts the number of nodes
  * (leaves and branches in a tree
  */
def size[A](tree:Tree[A]):Int = {
  def internalCount[A](t:Tree[A], acc:Int):Int = t match {
    case Leaf(_) => acc + 1
    case Branch(left, right) => internalCount(left, internalCount(right, acc + 1))
  }
  internalCount(tree, 0)
}
var l2 = Leaf(1)
var r2 = Leaf(2)
var l3 = Leaf(3)
var r3 = Leaf(4)
var l1 = Branch(l2,r2)
var r1 = Branch(l3,r3)
var tree = Branch(l1,r1)

var res23 = size(tree)

/**
  * Write a function maximum that returns
  * the maximum element in a Tree[Int]
  * Note: In scala, you can use x.max(y)
  * or x max y to compute the maximum of
  * two integers
  */

def maximum(tree:Tree[Int]):Int = {
  def l(t:Tree[Int], acc:Int):Int = t match {
    case Leaf(v) => acc max v
    case Branch(left, right) => l(left, l(right, acc))

  }
  l(tree, -1)
}

var res24 = maximum(tree)

/**
  * Write a function depth that returns the maximum
  * path length from the root of a tree to any leaf
  */
def depth[A](tree:Tree[A],leaf:Leaf[A]):Int = {
  def l(t:Tree[A], acc:Int):Int = {
    t match {
      case Leaf(v) => {
        if (v == leaf.value) acc + 1
        else 0
      }
      case Branch(left, right) => 1 + (l(left, acc) max  l(right, acc))
    }
  }
  l(tree,0)
}

var res25 = depth(tree,r3)

/**
  * Write a function map analogous to the method of the same
  * name on list, that modify each element of a tree with
  * a given function
  */
def mapTree[A](l:Tree[A])(f:A=>A):Tree[A] = l match {
  case Leaf(v) => Leaf(f(v))
  case Branch(left, right) => Branch(mapTree(left)(f),mapTree(right)(f))
}

var res26 = mapTree(tree)(x=>(x+1))

/**
  * Generalize size, maximum, depth, and map, writing
  * a new function fold that abstracts over their similarities.
  * Reimplement them in terms of this more general function.
  * Can you draw an analogy betwen this fold function and the left
  * and right folds for list
  */
def fold[A](l:Tree[A])(f:A=>Int)(g:(Int, Int)=>Int):Int = l match {
  case Leaf(v) => f(v)
  case Branch(left, right) => g(fold(left)(f)(g),fold(right)(f)(g))
}

var size = fold(tree)(x => 1)((i,j) => (i+j+1))

var maximum = fold(tree)(x => x)((i,j) => i max j)

var depth = fold(tree)(x => {
  if (x == 4) 1
  else 0
})((i,j) => (1 + (i max j)))
