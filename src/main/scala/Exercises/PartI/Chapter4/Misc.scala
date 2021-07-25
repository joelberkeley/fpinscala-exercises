package Exercises.PartI.Chapter4

object Misc {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 4.2
  /** Return the variance of a sequence of numbers
    *
    * @param xs sequence
    * @return variance
    */
  def variance(xs: Seq[Double]): Option[Double] = {
    val mean_squared_diff = (m: Double) => xs.map(x => math.pow(x - m, 2)).sum / xs.length

    mean(xs).map(mean_squared_diff)
  }

  /** Return None if specified List contains None, else a List of the unpacked elements, wrapped in Some.
    *
    * Iteratively fold Options in the List into an Option. For Some elements in `a`, prepend the element to the target
    * Option's List. For None elements, the target Option will collapse to None for all further elements of `a`.
    *
    * @param a input List
    * @tparam A type parameter of List's Options
    * @return None if None is in `a`, else a List of all the elements in `a` packed in Some.
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  /**
    *
    * @param a
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]])((e, res) => f(e).map2(res)(_ :: _))
}
