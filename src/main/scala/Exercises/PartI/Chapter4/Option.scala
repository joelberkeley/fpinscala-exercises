package Exercises.PartI.Chapter4

sealed trait Option[+A] {
  // Exercise 4.1
  /** Apply a function to the Option's value
    *
    * @param f function to apply
    * @tparam B return type of function
    * @return Option of the new (mapped) value
    */
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case _ => None
  }

  // Exercise 4.1
  /** Apply a function to the Option's value, then flatten the Option
    *
    * @param f function to apply
    * @tparam B type of value in function's returned Option
    * @return flattened Option once function applied
    */
  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  // Exercise 4.1
  /** Get the Option's value if existent, else return the default
    *
    * @param default value to return if None found
    * @tparam B common supertype of Option's value's type, and default
    * @return Option's value or default
    */
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case _ => default
  }

  // Exercise 4.1
  /** Replace an empty Option with another Option
    *
    * @param ob alternative Option
    * @tparam B common supertype of both Options' values' types
    * @return chosen Option
    */
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case _ => ob
  }

  // Exercise 4.1
  /** Convert Some to None if its value doesn't satisfy a predicate
    *
    * @param f predicate
    * @return filtered Option
    */
  def filter(f: A => Boolean): Option[A] = if (map(f).getOrElse(false)) this else None

  // Exercise 4.3
  /**
    *
    * @param b
    * @param f
    * @tparam B
    * @tparam C
    * @return
    */
  def map2[B, C](b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      x <- this
      y <- b
    } yield f(x, y)
}

/** Copied from the book */
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
