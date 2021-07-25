package Exercises.PartI.Chapter4

object Either {
  /**
    *
    * @param es
    * @return
    */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  /**
    *
    * @param as
    * @param f
    * @return
    */
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(Nil): Either[E, List[B]])((list_element, res) => f(list_element).map2(res)(_ :: _))
}

trait Either[+E, +A] {
  /** If `this` is a Right, applies `f` to its contents and returns the result in a Right. Otherwise returns `this`.
    *
    * @param f function to apply
    */
  def map[B](f: A => B): Either[E, B] = this.flatMap(a => Right(f(a)))

  /** If `this` is a Right, applies `f` to its contents and returns the result. Otherwise returns `this`.
    *
    * @param f function to apply
    */
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

  /** Returns `this` if `this` is a Right. Otherwise returns `b`.
    *
    * @param b The Either to default to.
    */
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => Right(a)
      case Left(_) => b
    }

  /**
    *
    * @param b Either to map with
    * @param f function to compose Right values
    * @return
    */
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
}

case class Right[+A](a: A) extends Either[Nothing, A]
case class Left[+E](e: E) extends Either[E, Nothing]

// todo question 4.8
