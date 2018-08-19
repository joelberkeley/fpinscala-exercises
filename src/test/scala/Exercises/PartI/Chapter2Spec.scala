import org.scalatest._
import Exercises.PartI.Chapter2._

class Chapter2Spec extends FlatSpec with Matchers {
  "fib" should "produce numbers in the Fibonacci sequence" in {
    fib(1) should be (0)
    fib(2) should be (1)
    fib(3) should be (1)
    fib(4) should be (2)
    fib(20) should be (4181)
  }

  it should "throw IllegalArgumentException if non-positive Int is passed" in {
    a [IllegalArgumentException] should be thrownBy {
      fib(0)
    }
    a [IllegalArgumentException] should be thrownBy {
      fib(-1)
    }
  }

  "isSorted" should "return true for sorted Arrays" in {
    isSorted(Array(), (x: Int, y: Int) => y >= x) should be (true)
    isSorted(Array(1), (x: Int, y: Int) => y >= x) should be (true)
    isSorted(Array(1, 2), (x: Int, y: Int) => y >= x) should be (true)
    isSorted(Array(2, 2), (x: Int, y: Int) => y >= x) should be (true)
    isSorted(Array(1, 2, 3), (x: Int, y: Int) => y >= x) should be (true)

    isSorted(Array(), (x: String, y: String) => y.length >= x.length) should be (true)
    isSorted(Array("lemur"), (x: String, y: String) => y.length >= x.length) should be (true)
    isSorted(Array("lemur", "possum"), (x: String, y: String) => y.length >= x.length) should be (true)
    isSorted(Array("lemur", "possum", "antelope"), (x: String, y: String) => y.length >= x.length) should be (true)
  }

  "isSorted" should "return false for unsorted Arrays" in {
    isSorted(Array(2, 1), (x: Int, y: Int) => y > x) should be (false)
    isSorted(Array(2, 3, 2), (x: Int, y: Int) => y > x) should be (false)

    isSorted(Array("possum", "lemur"), (x: String, y: String) => y.length > x.length) should be (false)
  }

  "curry" should "convert a two-argument function to a curried form" in {
    val add = curry((x: Int, y: Int) => x + y)

    add(0)(1) should be (1)
    add(0)(-1) should be (-1)
    add(0)(0) should be (0)

    val concat = curry((s1: String, s2: String) => s1 + s2)

    concat("a")("b") should be ("ab")

    val embed = curry((s: String, x: Int) => Array(s, x.toString))

    embed("a")(1) should be (Array("a", "1"))
  }

  "uncurry" should "convert a two-argument curried function to standard form" in {
    val add = uncurry((x: Int) => (y: Int) => x + y)

    add(0, 1) should be (1)
    add(0, -1) should be (-1)
    add(0, 0) should be (0)

    val concat = uncurry((s1: String) => (s2: String) => s1 + s2)

    concat("a", "b") should be ("ab")

    val embed = uncurry((s: String) => (x: Int) => Array(s, x.toString))

    embed("a", 1) should be (Array("a", "1"))
  }

  "compose" should "return a function which is a composition of two functions" in {
    // Array to Int to String
    compose((x: Int) => x.toString, (a: Array[Int]) => a.length)(Array(1, 2, 3)) should be ("3")

    // Array[Int] to Array[Int] to Int
    compose((a: Array[Int]) => a.sum, (b: Array[Int]) => b.map(_ + 1))(Array(1, 2, 3)) should be (9)

    // Int to String to Array[String]
    compose((s: String) => s.split(""), (x: Int) => x.toString)(123) should be (Array("1", "2", "3"))
  }
}
