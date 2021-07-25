package Exercises.PartI

import scala.annotation.tailrec

object Chapter2 {
  // Exercise 2.1
  /** Return the nth number in the Fibonacci sequence. Indices start at 1
    *
    * @param n Index of number in sequence (starting at 1)
    * @throws IllegalArgumentException if index provided is not positive
    * @return Fibonacci number
    */
  def fib(n: Int): Int = {
    // I'd use an Option[Int] return value, but the question asks for an Int return value
    require(n > 0, "Invalid sequence index provided, must be positive")

    @tailrec
    def go(a: Int, b: Int, count: Int): Int = {
      if (count == n) b
      else go(b, a + b, count + 1)
    }

    if (n == 1) 0 else go(0, 1, 2)
  }

  // Exercise 2.2
  /** Return whether array is sorted according to given comparison function
    *
    * @param as Array to check
    * @param ordered comparison function for pairs of array elements
    * @tparam A type of Array elements
    * @return whether Array elements are sorted
    */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(i: Int): Boolean = {
      if (!ordered(as(i), as(i + 1)))
        false
      else if (i + 2 == as.length)
        true
      else
        go(i + 1)
    }

    as.length < 2 || go(0)
  }

  // Exercise 2.3
  /** Takes a function with a single list of two arguments and returns an equivalent function with the arguments now
    * accepted as two lists of single arguments (i.e. curried).
    *
    * @param f function to curry
    * @tparam A type of first argument to passed function
    * @tparam B type of second argument to passed function
    * @tparam C return type of passed function
    * @return new function with arguments curried
    */
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => f(a, _)

  // Exercise 2.4
  /** The opposite of `curry`
    *
    * @param f function to uncurry
    * @tparam A type of first argument to passed function
    * @tparam B type of second argument to passed function
    * @tparam C return type of passed function
    * @return new function with arguments in single list
    */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = f(_)(_)

  // Exercise 2.5
  // For this exercise we are not allowed to use the library `compose` function
  /** Returns a function s.t. compose(f,g)(x) == f(g(x))
    *
    * @param f second function to apply
    * @param g first function to apply
    * @tparam A argument type of g
    * @tparam B return type of g and argument type of f
    * @tparam C return type of f
    * @return composed function
    */
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
}
