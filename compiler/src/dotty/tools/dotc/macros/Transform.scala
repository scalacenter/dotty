package dotty.tools.dotc
package macros

import core._
import Contexts.Context
import Decorators._
import ast._
import Trees._
import util._
import Names._
import StdNames._
import Flags._
import typer.ErrorReporting._
import Constants._
import reporting.diagnostic.NoExplanation
import reporting.diagnostic.Message._
import config.Printers.{ macros => debug }

/** Transform macros definitions
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
 *    object main$macro {
 *      @static def f(prefix: Tree)(T: Type)(a: tpd.Term)(b: tpd.Term)(implicit m: Mirror): Term = body
 *    }
 */

private[macros] object Transform {
  import untpd._

  val IMPL_SUFFIX = str.NAME_JOIN + "macro"

  /** Transform macros definitions inside class definitions (see the note above)
   */
  def transform(tree: TypeDef)(implicit ctx: Context): Tree = {
    if (!tree.isClassDef) return tree

    val tmpl = tree.rhs.asInstanceOf[Template]

    val macros = getMacros(tmpl)
    if (macros.isEmpty) return tree

    val moduleName = tree.name + IMPL_SUFFIX
    val implObj = createImplObject(moduleName, macros)
    val implCls = TypeDef(moduleName.toTypeName, Template(emptyConstructor, Nil, EmptyValDef, Nil)).withPos(tree.pos) // required by @static

    val treeNew = cpy.TypeDef(tree)(rhs = newTemplate(tmpl, macros))

    debug.println(i"macro definition: $tree transformed to { $treeNew; \n ${implObj} }")

    Thicket(List(treeNew, implObj, implCls))
  }

  /** Transform macros definitions inside object definitions (see the note above)
   */
  def transform(tree: ModuleDef)(implicit ctx: Context): Tree = {
    val macros = getMacros(tree.impl)
    if (macros.isEmpty) return tree

    val moduleName = tree.name + nme.MODULE_SUFFIX.toString + IMPL_SUFFIX
    val implObj = createImplObject(moduleName, macros)
    val implCls = TypeDef(moduleName.toTypeName, Template(emptyConstructor, Nil, EmptyValDef, Nil)).withPos(tree.pos) // required by @static

    val treeNew = cpy.ModuleDef(tree)(impl = newTemplate(tree.impl, macros), name = tree.name)

    debug.println(i"macro definition: $tree transformed to { $treeNew; \n ${implObj} }")

    Thicket(List(treeNew, implObj, implCls))
  }


  def newTemplate(tmpl: Template, macros: List[DefDef])(implicit ctx: Context): Template = {
     // modify macros body and flags
    val macrosNew = macros.map { m =>
      val mdef = cpy.DefDef(m)(rhs = Ident("???".toTermName))
      if (mdef.mods.is(Inline))
        ctx.warning(new NoExplanation("inline ignored on macro definition"), mdef.pos)
      mdef.withMods((mdef.mods | Macro) &~ Inline )
    }

    val bodyNew = macrosNew ++ tmpl.body.diff(macros)
    cpy.Template(tmpl)(body = bodyNew)
  }

  def getMacros(tmpl: Template)(implicit ctx: Context): List[DefDef] = tmpl.body.filter {
    case mdef : DefDef =>
      mdef.rhs match {
        case Apply(Ident(nme.`macro`), _) => true
        case _ => false
      }
    case _ => false
  }.asInstanceOf[List[DefDef]]


  /** Create macro implementation method
   *
   *     def f[T](a: A)(b: B): C = macro { body }
   *
   *  will be implemented by
   *
   *    @static
   *    def f(prefix: tpd.Term)
   *         (T: Type)
   *         (a: tpd.Term)(b: tpd.Term)(implicit mirror: Mirror): Term = body
   *
   *  with `this` replaced by `prefix` in `body`
   *
   */
  private def createImplMethod(defn: DefDef)(implicit ctx: Context): DefDef = {
    val Apply(_, rhs :: Nil) = defn.rhs

    val tb = Select(Ident("scala".toTermName), "macros".toTermName)
    val resType = Select(tb, "Term".toTypeName)
    val termType = Select(Select(tb, "tpd".toTermName), "Term".toTypeName)
    val typeType = Select(tb, "Type".toTypeName)
    val mirrorType = Select(tb, "Mirror".toTypeName)

    val prefix = ValDef("prefix".toTermName, termType, EmptyTree).withFlags(TermParam)

    val typeParams = for (tdef: TypeDef <- defn.tparams)
      yield ValDef(tdef.name.toTermName, typeType, EmptyTree).withFlags(TermParam)

    val termParamss = for (params <- defn.vparamss)
      yield params.map { case vdef: ValDef =>
        ValDef(vdef.name.toTermName, termType, EmptyTree).withMods(vdef.mods | TermParam)
      }

    val mirror = ValDef("m".toTermName, mirrorType, EmptyTree).withFlags(TermParam | Implicit)

    val paramss =
      if (typeParams.size > 0) List(prefix) :: typeParams :: (termParamss :+ List(mirror))
      else List(prefix) :: (termParamss :+ List(mirror))

    // replace `this` with `prefix`
    val mapper = new UntypedTreeMap {
      override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
        case tree: This =>
          Ident("prefix".toTermName).withPos(tree.pos)
        case _ =>
          super.transform(tree)
      }
    }

    val body = mapper.transform(rhs)
    val static = Apply(Select(New(ref(ctx.definitions.ScalaStaticAnnotType).withPos(defn.pos)), nme.CONSTRUCTOR), Nil)
    val mods = EmptyModifiers.withAddedAnnotation(static).withFlags(Synthetic)
    DefDef(defn.name, Nil, paramss, resType, body).withMods(mods)
  }

  /** create object A$inline to hold all macros implementations for class A */
  def createImplObject(name: String, macros: List[DefDef])(implicit ctx: Context): Tree = {
    val methods = macros.map(createImplMethod(_))
    val constr = DefDef(nme.CONSTRUCTOR, Nil, Nil, TypeTree(), EmptyTree)
    val templ = Template(constr, Nil, EmptyValDef, methods)
    ModuleDef(name.toTermName, templ)
  }
}
