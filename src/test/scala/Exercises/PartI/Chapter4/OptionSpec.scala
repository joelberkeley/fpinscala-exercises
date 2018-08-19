import org.scalatest._
import Exercises.PartI.Chapter4._

class OptionSpec extends FlatSpec with Matchers {
  "map" should "apply a function to an Option's value" in {
    Some(1).map(_ + 1) should be (Some(2))
    (None: Option[Int]).map(_ + 1) should be (None)

    Some("cat").map(_.length) should be (Some(3))
    (None: Option[String]).map(_.length) should be (None)

    Some(1).map(_.toString) should be (Some("1"))
    (None: Option[Int]).map(_.toString) should be (None)
  }

  "flatMap" should "apply a function to an Option's value then flatten the result" in {
    def toOption(x: Int) = x match {
      case 1 => Some(3)
      case 2 => None
    }

    Some(1).flatMap(toOption) should be (Some(3))
    Some(2).flatMap(toOption) should be (None)
    (None: Option[Int]).flatMap(toOption) should be (None)
  }

  "getOrElse" should "get an Option's value if existent, else return the default" in {
    Some(1).getOrElse(2) should be (1)
    None.getOrElse(2) should be (2)
    Some(1).getOrElse(1) should be (1)

    class A
    class B extends A

    val a = new A()
    val b = new B()

    Some(a).getOrElse(b) should be (a)
    None.getOrElse(b) should be (b)
  }

  "orElse" should "replace an empty Option with another Option" in {
    Some(1).orElse(Some(2)) should be (Some(1))
    Some(1).orElse(None) should be (Some(1))
    None.orElse(Some(2)) should be (Some(2))
    None.orElse(None) should be (None)

    Some(1).orElse(Some(1)) should be (Some(1))

    class A
    class B extends A

    val a = new A()
    val b = new B()

    Some(a).orElse(Some(b)) should be (Some(a))
    None.orElse(Some(b)) should be (Some(b))
  }

  "filter" should "convert Some to None if its value doesn't satisfy a predicate" in {
    Some(1).filter(_ > 0) should be (Some(1))
    Some(0).filter(_ > 0) should be (None)
    Some(-1).filter(_ > 0) should be (None)
    (None: Option[Int]).filter(_ > 0) should be (None)
  }
}
