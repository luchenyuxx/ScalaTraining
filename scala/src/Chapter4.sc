
/**
  * Implement all functions on Option
  * @tparam A
  */
sealed trait Option[+A] {
  def map[B](f:A=>B):Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  def flatMap[B](f:A=>Option[B]):Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }
  def getOrElse[B>:A](default: => B):B = this match {
    case None => default
    case Some(b) => b
  }

  def orElse[B>:A](ob: =>Option[B]): Option[B] = this match {
    case None => ob
    case Some(b) => Some(b)
  }

  def filter(f:A=> Boolean): Option[A] = this match {
    case None => None
    case Some(a) => {
      if(f(a)) Some(a)
      else None
    }

  def mean(xs:Seq[Double]):Option[Double] =
     if (xs.isEmpty) None
     else Some(xs.sum/xs.length)

  }



  case class Some[+A] (get:A) extends Option[A]
  case object None extends Option[Nothing]
}








