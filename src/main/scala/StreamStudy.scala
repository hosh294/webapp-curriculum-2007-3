trait LazyListStudy[+A] {

  def headOption: Option[A] = this match {
    case EmptyLazyList => None
    case Cons(h, _) => Some(h())
  }

  def tail: LazyListStudy[A] = this match {
    case EmptyLazyList => throw new Error()
    case Cons(_, t) => t()
  }
}

case object EmptyLazyList extends LazyListStudy[Nothing]

case class Cons[+A](h: () => A, t: () => LazyListStudy[A]) extends LazyListStudy[A]

object LazyListStudy {

  def cons[A](h: => A, t: => LazyListStudy[A]): LazyListStudy[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: LazyListStudy[A] = EmptyLazyList

}
