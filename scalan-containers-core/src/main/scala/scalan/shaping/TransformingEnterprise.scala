//package scalan.staged
//
//import scalan.ScalanEnterpriseExp
//
//trait TransformingEnterprise { self: ScalanEnterpriseExp =>
//  implicit class RuleRewriter(rule: RewriteRule) extends Rewriter {
//    def apply[T](x: Exp[T]): Exp[T] = rule.rewrite(x) match {
//      case Some(y) => y
//      case None => x
//    }
//  }
//}
