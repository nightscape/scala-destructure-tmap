/*
Was soll rauskommen:

+ Goals bekommen nur eine I => O
  + I und O korrekt getagged, z.B. Double @@ Position => Double @@ Cost
- Je nachdem, was mein Start-Input ist, soll I sein
  - Double @@ Position
  - Double @@ Bid
+ Generieren der notwendigen Predictor-Funktionen passiert automatisch
- Zusammenstöpseln der notwendigen Teilfunktionen passiert automatisch
+ Implicits stöpseln keine Daten, sondern nur Stateless, Dataless Typeclasses



Fragen:
- Wo werden Daten und Typeclasses gejoined?
  - Muss vor dem Befüttern der Goals sein
  - Sollte nach dem Learning sein (Learning liefert nur die notwendigen Parameter)
- Wo werden die Tags gesetzt?
 */

import Tagging._

import scala.reflect.runtime.universe._

sealed trait Kpi

class Impressions extends Kpi

class Clicks extends Kpi

class Ctr extends Kpi

class ConversionRate extends Kpi

class Conversions extends Kpi

class Position extends Kpi

class Bid extends Kpi

trait CanCreatePredictor {
  type P
  type I
  type O
  def createPredictor(p: P): I => O
}

class LinearPredictor(val m: Double, val b: Double) extends (Double => Double) {
  def apply(x: Double) = m * x + b
  override def toString = s"LinearPredictor(m = $m, b = $b)"
}

class ExponentialPredictor(val e: Double) extends (Double => Double) {
  def apply(x: Double) = scala.math.pow(e, x)
  override def toString = s"ExponentialPredictor(e = $e)"
}

case class LinearModelParameters(val m: Double, val b: Double)

case class ExponentialModelParameters(val e: Double)

object TMapDeconstruction extends App {

  implicit def linearModelParameters2Predictor[ITag, OTag] = new CanCreatePredictor {
    type P = LinearModelParameters @@ (ITag, OTag)
    type I = Double @@ ITag
    type O = Double @@ OTag
    def createPredictor(p: P) = {
      val pred = new LinearPredictor(p.m, p.b)
      val f = new Function1[Double @@ ITag, Double @@ OTag] {
        def apply(i: (Double @@ ITag)) = new Taggable(pred(i)).tag[OTag]
        override def toString: String = pred.toString()
      }
      f
    }
  }

  implicit def exponentialModelParameters2Predictor[ITag, OTag] = new CanCreatePredictor {
    type P = ExponentialModelParameters @@ (ITag, OTag)
    type I = Double @@ ITag
    type O = Double @@ OTag
    def createPredictor(p: P) = {
      val predictor = new ExponentialPredictor(p.e)
      new (Double @@ ITag => Double @@ OTag) {
        def apply(i: (Double @@ ITag)) = new Taggable(predictor(i)).tag[OTag]
        override def toString: String = predictor.toString()
      }
    }
  }


  implicit def clicksFromImpressionsAndCtr[ImpParam, CtrParam, IType, OType, ITag](
    implicit canCreateImpressionPredictor: CanCreatePredictor {
      type P = ImpParam @@ (ITag, Impressions)
      type I = IType @@ ITag
      type O = OType @@ Impressions
    }, canCreateCtrPredictor: CanCreatePredictor {
      type P = CtrParam @@ (ITag, Ctr)
      type I = IType @@ ITag
      type O = OType @@ Ctr
    }) = new CanCreatePredictor {

    type P = (ImpParam @@ (ITag, Impressions), CtrParam @@ (ITag, Ctr))
    type I = IType @@ ITag
    type O = OType @@ Clicks

    def createPredictor(p: P): I => O = {
      val impPred = canCreateImpressionPredictor.createPredictor(p._1)
      val ctrPred = canCreateCtrPredictor.createPredictor(p._2)
      new (IType @@ ITag => OType @@ Clicks) {
        def apply(i: IType @@ ITag) = impPred(i).untag.tag[Clicks]
        override def toString = s"Composite Predictor from $impPred and $ctrPred"
      }
    }

  }
  implicit class RichParams[MP, IType, OType, ITag, OTag](p: MP)
  (implicit ccp: CanCreatePredictor { type P = MP ; type I = IType @@ ITag; type O = OType @@ OTag}) {
    def toPredictor: IType @@ ITag => OType @@ OTag = ccp.createPredictor(p)
  }

  val position2impression = LinearModelParameters(1.0, 2.0).tag[(Position, Impressions)]
  val position2ctr = ExponentialModelParameters(3.0).tag[(Position, Ctr)]
  val position2impressionPredictor: (Double @@ Position) => (Double @@ Impressions) = position2impression.toPredictor
  val position2ctrPredictor: (Double @@ Position) => (Double @@ Ctr) = position2ctr.toPredictor
  import scala.reflect.runtime.universe.TypeTag
  def typeString[A](a: A)(implicit evA: TypeTag[A]) = evA.toString()



  println(typeString((position2impression, position2ctr).toPredictor))
  println(typeString(position2impressionPredictor))
  println(position2ctrPredictor)
  println(position2impressionPredictor.apply(2.0.tag[Position]))
}
