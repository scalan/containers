package scalan.shaping

import scala.language.reflectiveCalls
import scalan._
import scalan.compilation.{DummyCompiler}
import scalan.containers.ContainersDslExp
import scalan.containers.ContainersTests

class LambdaShapingTests extends ContainersTests {
  trait MyProg extends OpticScalanCtx with CollectionSamples {
  }

  class Ctx extends TestCompilerContext {
    class ScalanCake extends ScalanDslExp with MyProg with ContainersDslExp {
      override val cacheElems = false
//      override def shouldUnpack(e: Elem[_]) = true
//      override def isInvokeEnabled(d: Def[_], m: Method) = true
    }
    override val compiler = new DummyCompiler(new ScalanCake)
//      with StructsCompiler[ScalanCtxExp with MyProg]
    import compiler.scalan._

    def testShape[A,B](name: String, f: Rep[A=>B]) = {
      val len = toRep(10)
      implicit val cF = new CollectionVectorizable(len, Nil)
      val l = liftLambdaSym[Int, Collection, Collection, Collection, A, B](f)
      emit(name, f, l)
    }

    def testShapeMap[A,B](name: String, f: Rep[A=>B]) = {
      val m = MMap.empty[String,Unit]
      implicit val cF = new MapVectorizable(m, Nil)
      val l = liftLambdaSym[String, Collection, Collection, ({type f[x] = MMap[String, x]})#f, A, B](f)
      emit(name, f, l)
    }

  }

  test("CollectionSamples") {
    val ctx = new Ctx
    import ctx.compiler.scalan._
    ctx.testShape("id", id)
    ctx.testShape("inc", inc)
    ctx.testShape("curred", curred)
  }

  test("MapSamples") {
    val ctx = new Ctx
    import ctx.compiler.scalan._
    ctx.testShapeMap("id", id)
    ctx.testShapeMap("inc", inc)
//    ctx.testShape("curred", curred)
  }

}

