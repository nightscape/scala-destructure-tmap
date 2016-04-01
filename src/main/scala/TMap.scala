import scala.reflect.runtime.universe._

/**
A type-indexed map implementation with no tricks.
You get what the type-system gives you, which already
covers many use cases.
*/
class TMap[+T] private(private val values: List[(Type, Any)]) {
  def apply[E >: T](implicit tt: TypeTag[E]): E = {
    // runtime reflection doesn't allow constant time element lookup
    values.find(_._1 <:< tt.tpe).get._2.asInstanceOf[E]
  }

  /**
  Concatenate two TMaps.
  Elements of the left one override
  */
  def ++[S](other: TMap[S])
    = new TMap[T with S](other.values ++ values)

  override def toString = "TMap("+values.toString+")"
}

object TMap {
  def apply[T](values: T)(implicit tt: TypeTag[T]) =
    new TMap[T](List(tt.tpe -> values))
}

class MMap private(private val values: List[(Type, Any)]) { self =>
  type T
  def apply[E >: T](implicit tt: TypeTag[E]): E = {
    // runtime reflection doesn't allow constant time element lookup
    values.find(_._1 <:< tt.tpe).get._2.asInstanceOf[E]
  }

  /**
  Concatenate two MMaps.
  Elements of the left one override
    */
  def ++[S](other: MMap): MMap { type T = self.T with other.T }
  = new MMap(other.values ++ values) { type T = self.T with other.T }
  import scala.reflect.runtime.universe._
  override def toString = s"MMap[${weakTypeTag[T]}]($values)"
}

object MMap {
  def apply[K](values: K)(implicit tt: TypeTag[K]) =
    new MMap(List(tt.tpe -> values)) { type T = K }
}