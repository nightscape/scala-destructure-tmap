object Tagging {
  trait Tagged[+V, +T]
  type @@[+V, +T] = V with Tagged[V, T]

  implicit class Taggable[V](val value: V) extends AnyVal {
    def tag[T] = value.asInstanceOf[V @@ T]

    def tagAs[T <: V @@ _] = value.asInstanceOf[T]
  }
  implicit class Untaggable[V, T](val tagged: V with Tagged[V, T]) {
    def untag: V = tagged
  }

  implicit def TaggedNumeric[V, T](implicit num: Numeric[V]) = new Numeric[V @@ T] {
    override def plus(x: @@[V, T], y: @@[V, T]): @@[V, T] = num.plus(x.untag, y.untag).tag[T]
    override def negate(x: @@[V, T]): @@[V, T] = num.negate(x.untag).tag[T]
    override def times(x: @@[V, T], y: @@[V, T]): @@[V, T] = num.times(x.untag, y.untag).tag[T]
    override def minus(x: @@[V, T], y: @@[V, T]): @@[V, T] = num.minus(x.untag, y.untag).tag[T]
    override def compare(x: @@[V, T], y: @@[V, T]): Int = num.compare(x.untag, y.untag)
    override def toDouble(x: @@[V, T]): Double = num.toDouble(x.untag)
    override def toFloat(x: @@[V, T]): Float = num.toFloat(x.untag)
    override def toInt(x: @@[V, T]): Int = num.toInt(x.untag)
    override def toLong(x: @@[V, T]): Long = num.toLong(x.untag)
    override def fromInt(x: Int): @@[V, T] = num.fromInt(x).tag[T]
  }

}