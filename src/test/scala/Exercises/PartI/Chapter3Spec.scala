package Exercises.PartI

import org.scalatest._
import Book.List
import Exercises.PartI.Chapter3._

class Chapter3Spec extends FlatSpec with Matchers {
  "tail" should "return the tail of a Book.List" in {
    tail(List(1)) should be (List())
    tail(List(1, 2, 3)) should be (List(2, 3))
  }

  it should "throw UnsupportedOperationException if empty Book.List is passed" in {
    a [UnsupportedOperationException] should be thrownBy {
      tail(List())
    }
  }

  "setHead" should "replace the head of a Book.List" in {
    setHead(List(1), 1) should be (List(1))
    setHead(List(1), 2) should be (List(2))
    setHead(List(1, 2), 3) should be (List(3, 2))

    class A
    class B extends A

    val a = new A()
    val b = new B()

    setHead(List(a), b) should be (List(b))
  }

  it should "throw UnsupportedOperationException if empty Book.List is passed" in {
    a [UnsupportedOperationException] should be thrownBy {
      setHead(List(), 1)
    }
  }

  "drop" should "drop the first n elements in a Book.List" in {
    drop(List(), 0) should be (List())
    drop(List(), 1) should be (List())
    drop(List(1), 1) should be (List())
    drop(List(1, 2), 0) should be (List(1, 2))
    drop(List(1, 2), 1) should be (List(2))
    drop(List(1, 2), 2) should be (List())
    drop(List(1, 2, 3), 2) should be (List(3))
    drop(List(1, 2, 3), 4) should be (List())
  }

  it should "throw IllegalArgumentException if non-positive Int is passed" in {
    a [IllegalArgumentException] should be thrownBy {
      drop(List(1), -1)
    }
  }

  "dropWhile" should "drop elements from the beginning of a Book.List while they satisfy a predicate" in {
    def isLessThan(x: Int)(y: Int) = y < x

    dropWhile(List(), isLessThan(2)) should be (List())
    dropWhile(List(1, 2, 3), isLessThan(2)) should be (List(2, 3))
    dropWhile(List(1, 2, 3), isLessThan(3)) should be (List(3))

    dropWhile(List("cat", "dog", "fish"), (s: String) => s.length % 2 != 0) should be (List("fish"))
    dropWhile(List("cat", "dog", "fish"), (s: String) => s.length % 2 == 0) should be (List("cat", "dog", "fish"))
  }

  "init" should "return all but the last element of a Book.List" in {
    init(List(1)) should be (List())
    init(List(1, 2)) should be (List(1))
    init(List("cat", "dog")) should be (List("cat"))
  }

  it should "throw UnsupportedOperationException called on empty Book.List" in {
    a [UnsupportedOperationException] should be thrownBy {
      init(List())
    }
  }

  "length" should "return the length of a Book.List" in {
    Chapter3.length(List()) should be (0)
    Chapter3.length(List(1)) should be (1)
    Chapter3.length(List(1, 2, 3)) should be (3)
    Chapter3.length(List("cat", "dog", "fish")) should be (3)
  }

  "foldLeft" should "fold a function across Book.List elements from last to head" in {
    foldLeft(List(): List[Int], 0)(_ + _) should be (0)
    foldLeft(List(1), 0)(_ + _) should be (1)

    foldLeft(List(1, 2), -1)(_ + _) should be (2)
    foldLeft(List(1, 2), 0)(_ + _) should be (3)
    foldLeft(List(1, 2), 1)(_ + _) should be (4)

    foldLeft(List(1, 2), -1)(_ * _) should be (-2)
    foldLeft(List(1, 2), 0)(_ * _) should be (0)
    foldLeft(List(1, 2), 1)(_ * _) should be (2)

    foldLeft(List(1, 2, 3, 4), 0)((x, y) => if (x > y) x else y) should be (4)
    foldLeft(List(1, 2, 3, 4), 0)((x, y) => if (x < y) x else y) should be (0)

    foldLeft(List("a", "b", "c"), "z")(_ + _) should be ("zabc")
  }

  "sum" should "sum elements of Book.List" in {
    sum(List()) should be (0)
    sum(List(2)) should be (2)
    sum(List(1, -2)) should be (-1)
    sum(List(1, -2, 7)) should be (6)
  }

  "product" should "return the product of Book.List elements" in {
    product(List()) should be (1.0)
    product(List(1.1)) should be (1.1)
    product(List(1.0, -2.0)) should be (-2.0)
    product(List(1.0, -2.0, 3.1)) should be (-6.2)
  }

  "lengthLeft" should "return the length of a Book.List" in {
    lengthLeft(List()) should be (0)
    lengthLeft(List(1)) should be (1)
    lengthLeft(List(1, 2, 3)) should be (3)
    lengthLeft(List("cat", "dog", "fish")) should be (3)
  }

  "reverse" should "reverse a Book.List" in {
    reverse(List()) should be (List())
    reverse(List(1)) should be (List(1))
    reverse(List(1, 2)) should be (List(2, 1))
  }

  "foldLeft2" should "fold a function across Book.List elements from last to head" in {
    foldLeft2(List(): List[Int], 0)(_ + _) should be (0)
    foldLeft2(List(1), 0)(_ + _) should be (1)

    foldLeft2(List(1, 2), -1)(_ + _) should be (2)
    foldLeft2(List(1, 2), 0)(_ + _) should be (3)
    foldLeft2(List(1, 2), 1)(_ + _) should be (4)

    foldLeft2(List(1, 2), -1)(_ * _) should be (-2)
    foldLeft2(List(1, 2), 0)(_ * _) should be (0)
    foldLeft2(List(1, 2), 1)(_ * _) should be (2)

    foldLeft2(List(1, 2, 3, 4), 0)((x, y) => if (x > y) x else y) should be (4)
    foldLeft2(List(1, 2, 3, 4), 0)((x, y) => if (x < y) x else y) should be (0)

    foldLeft2(List("a", "b", "c"), "z")(_ + _) should be ("zabc")
  }

  "foldRight2" should "fold a function across Book.List elements from last to head" in {
    foldRight2(List(): List[Int], 0)(_ + _) should be (0)
    foldRight2(List(1), 0)(_ + _) should be (1)

    foldRight2(List(1, 2), -1)(_ + _) should be (2)
    foldRight2(List(1, 2), 0)(_ + _) should be (3)
    foldRight2(List(1, 2), 1)(_ + _) should be (4)

    foldRight2(List(1, 2), -1)(_ * _) should be (-2)
    foldRight2(List(1, 2), 0)(_ * _) should be (0)
    foldRight2(List(1, 2), 1)(_ * _) should be (2)

    foldRight2(List(1, 2, 3, 4), 0)((x, y) => if (x > y) x else y) should be (4)
    foldRight2(List(1, 2, 3, 4), 0)((x, y) => if (x < y) x else y) should be (0)

    foldRight2(List("a", "b", "c"), "z")(_ + _) should be ("abcz")
  }

  "append" should "join two Book.Lists" in {
    append(List(), List()) should be (List())
    append(List(1), List()) should be (List(1))
    append(List(1), List(2)) should be (List(1, 2))
    append(List(1), List(2, 3)) should be (List(1, 2, 3))
    append(List(1, 2), List(3)) should be (List(1, 2, 3))
    append(List(List(1)), List(List(2))) should be (List(List(1), List(2)))
  }

  "flatten" should "flatten a Book.List by one level" in {
    flatten(List(List())) should be (List())
    flatten(List(List(1))) should be (List(1))
    flatten(List(List(1), List(2))) should be (List(1, 2))
    flatten(List(List(List(1)), List(List(2)))) should be (List(List(1), List(2)))
  }
}
