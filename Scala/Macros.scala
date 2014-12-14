
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context


object Macros {
  def printf(format: String, params: Any*): Unit = macro printfImpl

  def printfImpl(c: Context)(format: c.Expr[String], params: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._

    val Literal(Constant(strFormat: String)) = format.tree
    val evals = ListBuffer[ValDef]()

    def precompute(value:Tree, tp: Type): Ident = {
      val freshName = TermName(c.freshName("eval$"))
      evals += ValDef(Modifiers(), freshName, TypeTree(tp), value)
      Ident(freshName)
    }

    val paramStack = mutable.Stack[Tree]((params map (_.tree)): _*)

    val refs = strFormat.split("(?<=%[\\w%])|(?=%[\\w%])") map {
      case "%d" => precompute(paramStack.pop, typeOf[Int])
      case "%f" => precompute(paramStack.pop, typeOf[Float])
      case "%s" => precompute(paramStack.pop, typeOf[String])
      case "%c" => precompute(paramStack.pop, typeOf[Char])
      case rest => Literal(Constant(rest))
    }

    val stats = evals ++ refs.map(ref => reify(print(c.Expr[Any](ref).splice)).tree)
    c.Expr[Unit](Block(stats.toList, Literal(Constant())))
  }
}
