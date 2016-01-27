import scala.reflect.runtime.universe._

object TMapDeconstruction extends App {
  trait Show[T] {
    def show(t: T): String
  }
  implicit val showInt = new Show[Int] {
    def show(t: Int) = s"Got int $t"
  }
  implicit val showString = new Show[String] {
    def show(t: String) = s"Got String $t"
  }

  implicit def showAllFromTMap[T, V](implicit tShow: Show[T], vShow: Show[V],
    ttt: TypeTag[T], ttv: TypeTag[V]) = new Show[TMap[T with V]] {
    def show(m: TMap[T with V]): String = tShow.show(m.apply[T]) + ", " + vShow.show(m.apply[V])
  }

  val m = (TMap(1) ++ TMap("foo"))
  println(implicitly[Show[TMap[Int with String]]].show(m))
}
