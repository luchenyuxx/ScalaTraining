
/**
  * Define a uniquePair function
  * which, according to an integer n,
  * generates the sequence of pairs (i,j)
  * with 1<j<i<n. Use uniquePair to simplify
  * the pairFirstSum function
  *
  */

def uniquePair(n:Int) = {
  (1 to n-2).map(x => (1 to n-1-x).map(y =>   print(List(x,x+y))))

}

uniquePair(5)

/**
  *
  * Write a function which retrieves all
  * ordered triplets of ints i, j and k
  * less or equals to an intger n and
  * which the sum is s
  */

def getOrderedTriplets(n:Int, s:Int): Unit = {
  (1 to n-2).map(x => (x+1 to n-1).map(y => if(s-x-y <= n) print(List(x,y,s-x-y))))
}


getOrderedTriplets(10,12)