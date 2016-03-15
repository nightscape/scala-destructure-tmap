import Tagging._

import scala.reflect.runtime.universe._

object TMapDeconstruction extends App {

  sealed class =!=[A, B]

  trait LowerPriorityImplicits {
    /** do not call explicitly! */
    implicit def equal[A]: =!=[A, A] = sys.error("should not be called")
  }

  object =!= extends LowerPriorityImplicits {
    /** do not call explicitly! */
    implicit def nequal[A, B]: =!=[A, B] = new =!=[A, B]
  }

  sealed trait Kpi

  class Impressions extends Kpi

  class Clicks extends Kpi

  class Ctr extends Kpi

  class ConversionRate extends Kpi

  class Conversions extends Kpi


  trait Show[T, O] {
    def show(t: T): O
  }

  implicit val showInt = new Show[Int, String] {
    def show(t: Int) = s"int $t"
  }
  implicit val showString = new Show[String, String] {
    def show(t: String) = s"String $t"
  }
  implicit val showImpressions = new Show[Double @@ Impressions, String @@ Impressions] {
    def show(t: Double @@ Impressions) = s"Impressions $t".tag[Impressions]
  }

  implicit val showCtr = new Show[Double @@ Ctr, String @@ Ctr] {
    def show(t: Double @@ Ctr) = s"Ctr $t".tag[Ctr]
  }
  implicit val showCr = new Show[Double @@ ConversionRate, String @@ ConversionRate] {
    def show(t: Double @@ ConversionRate) = s"Cr $t".tag[ConversionRate]
  }

  implicit def showClicks[I, C](implicit showImpressions: Show[I @@ Impressions, String @@ Impressions],
                                showCtr: Show[C @@ Ctr, String @@ Ctr],
                                tti: TypeTag[I @@ Impressions],
                                ttc: TypeTag[C @@ Ctr]
                               ) = new Show[TMap[(I @@ Impressions) with (C @@ Ctr)], String @@ Clicks] {
    def show(t: TMap[(I @@ Impressions) with (C @@ Ctr)]): String @@ Clicks =
      s"""Clicks from ${showImpressions.show(t.apply[I @@ Impressions])}
          |and ${showCtr.show(t.apply[C @@ Ctr])}
       """.stripMargin.tag[Clicks]
  }

  implicit def showConversions(
                                implicit
                                showClicks: Show[TMap[(Double @@ Impressions) with (Double @@ Ctr)], String @@ Clicks],
                                showCr: Show[Double @@ ConversionRate, String @@ ConversionRate]) = new Show[TMap[(Double @@ Impressions) with (Double @@ Ctr) with (Double @@ ConversionRate)], String @@ Conversions] {
    override def show(t: TMap[(Double @@ Impressions) with (Double @@ Ctr) with (Double @@ ConversionRate)]): @@[String, Conversions] =
      s"""Conversions from ${showClicks.show(TMap(t.apply[Double @@ Impressions]) ++ TMap(t.apply[Double @@ Ctr]))}
          |and ${showCr.show(t.apply[Double @@ ConversionRate])}
       """.stripMargin.tag[Conversions]
  }

  val p = TMap(0.5.tag[Impressions]) ++ TMap(0.3.tag[Ctr]) ++ TMap(0.05.tag[ConversionRate])
  println(implicitly[Show[TMap[(Double @@ Impressions) with (Double @@ Ctr)], String @@ Clicks]].show(p))
  println(implicitly[Show[TMap[(Double @@ Impressions) with (Double @@ Ctr) with (Double @@ ConversionRate)], String @@ Conversions]].show(p))

}
