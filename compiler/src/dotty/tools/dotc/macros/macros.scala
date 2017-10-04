package dotty.tools.dotc

import core._
import Contexts.Context
import Symbols._
import Types._
import Decorators._
import ast._
import StdNames._
import typer.ErrorReporting._
import config.Printers.{macros => debug}
import typer.{ForceDegree, Inferencing, Typer}

package object macros {
  import untpd._

  private class Proxy(clazz: Class[_], module: AnyRef) {
    def apply(name: String, params: Object*) = {
      val method = clazz.getDeclaredMethods().find(_.getName == name).get
      method.setAccessible(true)
      method.invoke(module, params: _*)
    }
  }

  private def forObject(qual: String)(implicit ctx: Context) = {
    val clazz = ctx.classloader.loadClass(qual + "$")
    val module = clazz.getField(str.MODULE_INSTANCE_FIELD.toString).get(null)
    new Proxy(clazz, module)
  }

  /** Whether the given symbol is a def macro? */
  @inline
  def isDefMacro(symbol: Symbol)(implicit ctx: Context): Boolean =
    symbol.is(Flags.Macro) && !symbol.owner.is(Flags.Scala2x)

  /** Expand def macros */
  def expandDefMacro(tree: tpd.Tree, pt: Type, typer: Typer)(implicit ctx: Context): tpd.Tree = {
    def isMacroApplyEnd: Boolean =
      (tree.isInstanceOf[tpd.Apply] || tree.isInstanceOf[tpd.TypeApply]) &&
        !tree.tpe.isError && tree.tpe.isValueType && macros.isDefMacro(tree.symbol)

    def expanded: Tree = {
      // instantiate tvars as much as possible before macro expansion
      tree match {
        case tapply: tpd.TypeApply =>
          tapply.args.map(arg => Inferencing.isFullyDefined(arg.tpe, ForceDegree.noBottom))
        case _ =>
      }

      val res = forObject("scala.macros.internal.engines.dotc.Expander").apply("expandDefMacro", tree, ctx).asInstanceOf[Tree]
      debug.println(i"def macro expansion: $tree expands to $res")
      res
    }

    // trees with errors are possibly not well formed, which could result in macro expansion exception
    // e.g. macros with missing implicit arguments will take the expected value type instead of ImplicitMethod
    if (!ctx.reporter.hasErrors && isMacroApplyEnd)
      if (ctx.macrosEnabled) typer.typed(expanded, pt)
      else errorTree(tree, s"can't expand macro, make sure `scala.gestalt` is in -classpath")
    else tree
  }

  /** Transform macro definitions in class definition
   *
   *  @param tree  the tree that may contain macro definition, the tree may be a thicket
   *  @returns     the transformed tree
   *
   *  @note The returned tree NEEDs desugaring
   *
   *  Macro definition is transformed from:
   *
   *    class macros {
   *      def f[T](a: A)(b: B): C = macro {
   *        body
   *      }
   *    }
   *
   *  to:
   *
   *    class main {
   *      <macro> def f[T](a: A)(b: B): C = ???
   *    }
   *
   *    object main$inline {
   *      @static def f(prefix: scala.macros.Term)
    *                  (T:      scala.macros.Type)
    *                  (a:      scala.macros.Term)
    *                  (b:      scala.macros.Term)
    *                  (m:      scala.macros.Mirror): scala.macros.Tree = body
   *    }
   */
  def transform(tree: Tree)(implicit ctx: Context): Tree = {
    tree match {
      case cdef: TypeDef if cdef.isClassDef =>
        Transform.transform(cdef)
      case mdef: ModuleDef =>
        Transform.transform(mdef)
      case thicket: Thicket =>
        Thicket(thicket.trees.map(transform))
      case _ =>
        tree
    }
  }
}
