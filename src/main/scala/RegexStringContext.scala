import scala.language.experimental.macros

import java.util.regex.{Pattern => JRegex}

import scala.reflect.macros.Context

import scala.util.matching.Regex

object RegexStringContext {
  implicit class StringContextExt(sc: StringContext) {
    def r(argExprs: String*): Regex = macro r_impl
  }

  def r_impl(c: Context)(argExprs: c.Expr[String]*): c.Expr[Regex] = {
    import c.universe._

    c.prefix.tree match {
      case Apply(_, List(scTree @ Apply(_, partTrees))) => {
        val partStrings = partTrees.map { case Literal(Constant(x: String)) => x }

        validateRegex(c, partStrings.mkString("X"))

        def mkSeqTree(xs: Seq[c.universe.Tree]): c.universe.Tree = {
          // TODO: Improve this somehow!
          Apply(TypeApply(Select(Select(Select(Ident(nme.ROOTPKG), newTermName("scala")), newTermName("collection")), newTermName("Seq")), List(Ident(newTypeName("String")))), xs.toList)
        }

        val scExpr = c.Expr[StringContext](scTree)
        val argListExpr = c.Expr[Seq[String]](mkSeqTree(argExprs.map(_.tree)))

        reify {
          new Regex(scExpr.splice.raw(argListExpr.splice.map(JRegex.quote): _*))
        }
      }
    }
  }

  private def validateRegex(c: Context, re: String) {
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
