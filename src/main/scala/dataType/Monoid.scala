package dataType

trait Monoid[A] {
  def plus(l: A, r: A): A

  val unit: A
}

type IntVector[Key] = Map[Key, Int]

class IntVectorMonoid[Key] extends Monoid[IntVector[Key]] {
  override def plus(l: IntVector[Key], r: IntVector[Key]): IntVector[Key] = {
    (l.keys ++ r.keys).map(key => (key, l(key) + r(key))).toMap
  }
  override val unit = Map()
}

implicit object StringMonoid extends Monoid[String] {
  override val unit: String = ""

  override def plus(l: String, r: String): String = l ++ r
}

class ListMonoid[A] extends Monoid[List[A]] {
  override def plus(l: List[A], r: List[A]): List[A] = l++r

  override val unit: List[A] = List()
}
