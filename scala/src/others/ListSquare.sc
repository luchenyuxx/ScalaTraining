
/**
  * The SquareList function takes a list of numbers as argument and
  * returns a list of square of these numbers
  *
  */

def squareList(list:List[Float]):List[Float] = {
  list map (x => x*x)
}

println(squareList(List(2,4,3)))


/**
  * Define a squareTree function which looks alike
  * the squareList function. This function behaves
  * like that
  *
  * (squareTree
  * (list 1
  * (list 2 (list 3 4) 5)
  * (list 6 7)))
  * (1 (4 (9 16) 25) (36 49))
  *
  * Define squareTree
  *
  */
