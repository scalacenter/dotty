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
      !tree.tpe.isError &&
        tree.isTerm &&
        tree.tpe.widen.widenExpr.isValueType &&
        macros.isDefMacro(tree.symbol)

    def expanded: untpd.Tree = {
      // instantiate tvars as much as possible before macro expansion
      tree match {
        case tapply: tpd.TypeApply =>
          tapply.args.map(arg => Inferencing.isFullyDefined(arg.tpe, ForceDegree.noBottom))
        case _ =>
      }

      val res = forObject("scala.macros.internal.engines.dotc.Expander").apply("expandDefMacro", tree, ctx).asInstanceOf[untpd.Tree]
      debug.println(i"def macro expansion: $tree expands to $res")
      res
    }

    // trees with errors are possibly not well formed, which could result in macro expansion exception
    // e.g. macros with missing implicit arguments will take the expected value type instead of ImplicitMethod
    if (!ctx.reporter.hasErrors && isMacroApplyEnd)
      if (ctx.macrosEnabled) typer.typed(expanded, pt)
      else errorTree(tree, s"can't expand macro, make sure `scala.macros` is in -classpath")
    else tree
  }

  /** Type check def macro
   *
   *  precondition: tparams and params must have already been typechecked
   *
   *  def foo[T](e: T) = macro impl[T]
   *  def impl(c: Context)(T: Type)(e: tpd.Term): Term
   *
   *  ==>
   *  @macroImpl(impl[T])
   *  def foo[T](e: T) = ???
   */
  def typecheck(ddef: untpd.DefDef, sym: Symbol, typer: Typer)(implicit ctx: Context): tpd.DefDef = {
    import Trees._

    val (implRef: tpd.Tree, tparams: List[tpd.Tree]) = ddef.rhs match {
      case Apply(Ident(nme.`macro`), TypeApply(implRef, targs) :: Nil) =>
        (typer.typedUnadapted(implRef), targs.map(typer.typedUnadapted(_)))
      case Apply(Ident(nme.`macro`), implRef :: Nil) =>
        (typer.typedUnadapted(implRef), Nil)
    }

    // TODO: add checking for matching of macro impl and decl
    if (!implRef.symbol.isStatic)
      ctx.error("macro implementation method must be accessible with a stable prefix", implRef.pos)

    val tree = untpd.Block(tparams, implRef).withType(defn.AnyType)
    val annot = Annotations.Annotation(tree)

    sym.addAnnotation(annot)
    sym.setFlag(Flags.Macro)

    typer.assignType(untpd.cpy.DefDef(ddef)(rhs = tpd.ref(defn.Predef_undefined)), sym)
  }
}
