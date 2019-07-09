
/**
  * Write a recursive function to get the nth fibonacci number. The first two Fibonacci numbers are 0 and 1.
  * The nth numbre is always the sum of the previous 2. The sequence begins 0, 1, 1, 2, 3, 5. Your definition
  * should use a local tail-recursive function.
  */
def fibonacci(input:Int):Int = {
  @annotation.tailrec
  def go(index:Int,minusOne:Int,minusTwo:Int):Int = {
    if (index<=0)  minusTwo
    else
      go(index-1, minusOne + minusTwo, minusOne)
  }
  go(input, 1,0);
}

fibonacci(5)

/**
  * Implement isSorted, which checks whether an Array[A] is sorted according
  * to a given comparison function
  */
def isSorted[A](as: Array[A], ordered:(A,A)=>Boolean):Boolean = {
  @annotation.tailrec
  def loop(index:Int):Boolean = {
    if (index == as.length - 1) true
    else if (!ordered(as(index), as(index + 1))) false
    else loop(index + 1)

  }
  loop(0)
}

isSorted(Array(1,12,3,5,7), (x:Int,y:Int)=>(x<=y))

/**
  * Let's look at another example, currying, which converts a function
  * f of two arguments
  * into a function of one argument that partially applies f.
  * Here again there's only one implementation
  * that complies. Write it
  */

def curry[A,B,C](f: (A,B) => C):A => (B => C) = {
  (a:A) => ((b:B) => f(a,b))
}

/**
  * Implement uncurry, which reverses the transformation of curry.
  * Note that since => associates to the right, A => (B => C)
  * can be written as A => B => C
  */

def uncurry[A,B,C](f:A => B => C): (A,B) => C = {
  (a:A, b:B) => (f(a)(b))
}

/**
  * Implement the higher-order function that composes 2 functions
  */
def compose[A,B,C](f: B => C, g: A => B): A => C = {
  (a:A) => f(g(a))
}