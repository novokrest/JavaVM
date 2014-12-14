object HList {

  trait HList {
    def ::[V](v: V): HList

    def foldr[B](f: (Any, B) => B, v: B): B

    def ++(h: HList): HList
  }

  case class HCons[H, T <: HList](head: H, tail: T) extends HList {
    override def ::[V](v: V): HList = new HCons(v, this)

    override def foldr[B](f: (Any, B) => B, v: B): B = f(head, tail.foldr(f, v))

    override def ++(h: HList): HList = head :: (tail ++ h)
  }

  object HNil extends HList {
    override def ::[V](v: V): HList = new HCons(v, this)

    override def foldr[B](f: (Any, B) => B, v: B): B = v

    override def ++(h: HList): HList = h
  }
}