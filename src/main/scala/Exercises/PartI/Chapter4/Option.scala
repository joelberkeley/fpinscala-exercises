package Exercises.PartI.Chapter4

sealed trait Option[+A] {
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

  /** Apply a function to the Option's value. Then flatten the Option
    *
    * @param f function to apply
    * @tparam B type of value in function's returned Option
    * @return flattened Option once function applied
    */
  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

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

  /** Convert Some to None if its value doesn't satisfy a predicate
    *
    * @param f predicate
    * @return filtered Option
    */
  def filter(f: A => Boolean): Option[A] = if (map(f).getOrElse(false)) this else None
}

/** Copied from the book */
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]