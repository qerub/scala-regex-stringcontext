import scala.language.experimental.macros

import java.util.regex.{Pattern => JRegex}

import scala.reflect.macros.Context

import scala.util.matching.Regex

object RegexStringContext {
  implicit class StringContextExt(sc: StringContext) {
    object r {
      def apply(argExprs: String*): Regex = macro r_apply_impl

      def unapplySeq(targetExpr: String): Option[Seq[String]] = macro r_unapplySeq_impl
    }
  }

  def r_apply_impl(c: Context)(argExprs: c.Expr[String]*): c.Expr[Regex] = {
    import c.universe._

    checkRegexSyntax(c, extractParts(c).mkString("X"))

    def mkSeqTree(xs: Seq[c.universe.Tree]): c.universe.Tree = {
      // TODO: Improve this somehow!
      Apply(TypeApply(Select(Select(Select(Ident(nme.ROOTPKG), newTermName("scala")), newTermName("collection")), newTermName("Seq")), List(Ident(newTypeName("String")))), xs.toList)
    }

    val scExpr = c.Expr[StringContext](extractStringContext(c))
    val argListExpr = c.Expr[Seq[String]](mkSeqTree(argExprs.map(_.tree)))

    reify {
      new Regex(scExpr.splice.raw(argListExpr.splice.map(JRegex.quote): _*))
    }
  }

  def r_unapplySeq_impl(c: Context)(targetExpr: c.Expr[String]): c.Expr[Option[Seq[String]]] = {
    import c.universe._

    val regex = extractParts(c).mkString("(.*)")

    checkRegexSyntax(c, regex)

    val regexExpr = c.Expr[String](Literal(Constant(regex)))

    reify {
      regexExpr.splice.r.unapplySeq(targetExpr.splice)
    }
  }

  private def extractStringContext(c: Context): c.universe.Tree = {
    import c.universe._

    c.prefix.tree match {
      case Select(Apply(_, List(scTree)), _) => scTree
    }
  }

  private def extractParts(c: Context): Seq[String] = {
    import c.universe._

    c.prefix.tree match {
      case Select(Apply(_, List(Apply(_, xs))), _) => xs map {
        case Literal(Constant(x: String)) => x
      }
    }
  }

  private def checkRegexSyntax(c: Context, re: String) {
    try {
      JRegex.compile(re)
    }
    catch {
      case e: java.util.regex.PatternSyntaxException => {
        c.abort(c.enclosingPosition, f"Syntax error in regex: ${e.getMessage}")
      }
    }
  }
}
