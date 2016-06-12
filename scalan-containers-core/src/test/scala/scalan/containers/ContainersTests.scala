package scalan.containers

import scalan.{Scalan, BaseViewTests}

abstract class ContainersTests extends BaseViewTests {
  trait OpticScalanCtx extends Scalan with ContainersDsl {
    def unRep[T](x: Rep[T])(implicit proxy: Rep[T] => T): T = x match {
      case Def(d) => d.asInstanceOf[T]
      case _ => x
    }

    val eSchema = structElement(Seq(
      "a" -> element[String],
      "b" -> element[Double]))
  }
}
