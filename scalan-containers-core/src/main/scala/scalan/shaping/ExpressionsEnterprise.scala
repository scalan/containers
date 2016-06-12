//package scalan.staged
//
//import scalan.ScalanEnterpriseExp
//
//trait BaseExpEnterprise extends BaseExp { self: ScalanEnterpriseExp =>
//  abstract class BaseDef[T](implicit elem: Elem[T]) extends super.BaseDef[T] {
//    def uniqueOpId: String = ""
//  }
//
//  def externalVars(body: Seq[TableEntry[_]]): Set[Exp[_]] = {
//    val bodySet = body.map(_.sym).toSet
//    val allDeps = body.flatMap({ case TableEntry(s,d) => d.getDeps }).toSet
//    val external = allDeps -- bodySet
//    external
//  }
//
//  protected def isGlobalConst(d: Def[_]): Boolean = d match {
//    case Const(_) => true
//    case _: Lambda[_, _] => false
////    case LoadPoint(_) => false
//    case _ => d.getDeps.forall {
//      case Def(d1) => isGlobalConst(d1)
//      case _ => false
//    }
//  }
//
//  override def rewriteDef[T](d: Def[T]): Exp[_] = {
//    rewriteRules.foreach(r =>
//      r.lift(d) match {
//        case Some(e) => return e
//        case _ =>
//      })
//    super.rewriteDef(d)
//  }
//
//  var rewriteRules = List[PartialFunction[Def[_], Exp[_]]]()
//
//  def addRewriteRules(rules: PartialFunction[Def[_], Exp[_]]*) {
//    rewriteRules ++= rules
//  }
//
//  trait RewriteRule {
//    def rewrite[T](d: Exp[T]): Option[Exp[T]]
//  }
//
////  implicit def rewriteRuleToPartialFunction(rule: RewriteRule) = new PartialFunction[Exp[_], Exp[_]] {
////    def isDefinedAt(s: Exp[_]) = rule.unapply(s).isDefined
////    def apply(s: Exp[_]) = rule.unapply(s) match {
////      case Some(args) => rule(args)
////      case None =>
////        println(s"rewriting error in $s")
////        s
////    }
////  }
//}