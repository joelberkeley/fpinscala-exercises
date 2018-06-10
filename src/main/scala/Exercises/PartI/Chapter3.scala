package Exercises.PartI

import scala.annotation.tailrec
import Book._

object Chapter3 {
  /** Returns the tail (all but the first element) of a Book.List. The tail of an empty list is an empty list
    *
    * Exercise 3.2
    *
    * @param l initial list
    * @tparam A type of list elements
    * @return list tail
    */
  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case Nil => throw new UnsupportedOperationException("Called tail on empty List")
  }

  /** Returns a new Book.List with a substituted head
    *
    * Exercise 3.3
    *
    * @param l initial list
    * @param head head of new list
    * @tparam A type of list elements
    * @tparam B type of new head
    * @return new list with substituted head
    */
  def setHead[A,B <: A](l: List[A], head: B): List[A] = l match {
    case Cons(_, t) => Cons(head, t)
    case Nil => throw new UnsupportedOperationException("Can't replace head of empty List")
  }

  /** Selects all elements except the first n ones
    *
    * Exercise 3.4
    *
    * @param l initial list
    * @param n number of elements to drop
    * @tparam A type of list elements
    * @throws IllegalArgumentException if n is negative
    * @return new list with n dropped elements
    */
  def drop[A](l: List[A], n: Int): List[A] = {
    require(n >= 0, "Can't drop negative number of elements from List")

    @tailrec
    def go(ll: List[A], m: Int): List[A] = {
      if (m == n || ll == Nil) ll
      else go(tail(ll), m + 1)
    }

    go(l, 0)
  }

  /** Drops all initial elements from a Book.List satisfying a predicate
    *
    * Exercise 3.5
    *
    * @param l initial list
    * @param f predicate dropped elements must satisfy
    * @tparam A type of list elements
    * @return all elements of list from the first (inclusive) not satisfying the predicate
    */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @tailrec
    def go(ll: List[A]): List[A] = ll match {
      case Nil => Nil
      case Cons(h, t) if !f(h) => Cons(h, t)
      case Cons(_, t) => go(t)
    }

    go(l)
  }

  /** Returns all but the last element of a Book.List
    *
    * Exercise 3.6
    *
    * This function is O(n) because the recursive call has to go through every element in the List to reach the last.
    *
    * This implementation can cause a stack overflow. A better implementation would perhaps be to reverse, tail then
    * reverse again, but that wouldn't be in the spirit of the question.
    *
    * @param l initial list
    * @tparam A type of list elements
    * @return all but the last element of given list
    */
  def init[A](l: List[A]): List[A] = {
    def go(ll: List[A]): List[A] = ll match {
      case Nil => throw new UnsupportedOperationException("Called init on empty List")
      case Cons(h, Nil) => Nil
      case Cons(h, Cons(_, Nil)) => Cons(h, Nil)
      case Cons(h, t) => Cons(h, go(t))
    }

    go(l)
  }

  /** Returns the length of a Book.List
    *
    * Exercise 3.9
    *
    * @param l our list
    * @tparam A type of list elements
    * @return list length
    */
  def length[A](l: List[A]): Int = List.foldRight(l, 0)((_, y) => 1 + y)

  /** Folds a function across list elements, returning result. Starts from last element and given default value
    *
    * Exercise 3.10
    *
    * @param as list to act on
    * @param z initial value
    * @param f function to
    * @tparam A type of list elements
    * @tparam B return type of folding function
    * @return result of fold
    */
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def go(l: List[A], acc: B): B = l match {
      case Nil => acc
      case Cons(x, xs) => go(xs, f(acc, x))
    }

    go(as, z)
  }

  /** Sum of a Book.List of numbers
    *
    * Exercise 3.11
    *
    * @param ints our list
    * @return sum of list elements
    */
  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  /** Returns the product of a Book.List of numbers
    *
    * Exercise 3.11
    *
    * @param ds our list
    * @return product of list elements
    */
  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  /** Returns the length of a Book.List
    *
    * Uses foldLeft
    *
    * Exercise 3.11
    *
    * @param l our list
    * @return length of list
    */
  def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => x + 1)

  /** Returns the reverse of a Book.List
    *
    * Exercise 3.12
    *
    * @param l our list
    * @return list reversed
    */
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))

  /** Alternative implementation of foldLeft in terms of foldRight
    *
    * Exercise 3.13
    *
    * @param as our list
    * @param z initial value
    * @param f function to fold with
    * @tparam A type of list elements
    * @tparam B type of initial value
    * @return result of fold
    */
  def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B = Book.List.foldRight(reverse(as), z)((x, y) => f(y, x))

  /** Alternative implementation of foldRight in terms of foldLeft
    *
    * Exercise 3.13
    *
    * @param as our list
    * @param z initial value
    * @param f function to fold with
    * @tparam A type of list elements
    * @tparam B type of initial value
    * @return result of fold
    */
  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((x, y) => f(y, x))

  /** Returns one Book.List appended to another Book.List
    *
    * Exercise 3.14
    *
    * @param a1 first list
    * @param a2 second list
    * @tparam A type of list elements
    * @return second list appended to end of first list
    */
  def append[A](a1: List[A], a2: List[A]): List[A] = Book.List.foldRight(a1, a2)(Cons(_, _))

  /** Returns a flattened Book.List
    *
    * {{{
    *   scala> Chapter3.flatten(List(List(1, 2), List(3))
    *   res0: Book.List[Int] = Cons(1,Cons(2,Cons(3,Nil)))
    * }}}
    *
    * Exercise 3.15
    *
    * @param l a list of lists
    * @tparam A type of elements of sublists
    * @return flattened list
    */
  def flatten[A](l: List[List[A]]): List[A] = Book.List.foldRight(l, Nil: List[A])(append)
}
