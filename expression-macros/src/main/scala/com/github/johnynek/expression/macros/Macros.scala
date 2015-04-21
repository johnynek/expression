package com.github.johnynek.expression.macros

import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.reflect.runtime.universe._
import com.github.johnynek.expression._

object Macros {
  def expFn[A, B](fn: A => B): ExpressionFn[A, B] = macro toExpFn[A, B]

  def toExpFn[A, B](c: Context)(
    fn: c.Expr[A => B])(implicit a: c.WeakTypeTag[A],
    b: c.WeakTypeTag[B]): c.Expr[ExpressionFn[A, B]] = {

    import c.universe._

    // This needs to be recursive and build up the scope
    // to be sure that there are no closures, or somehow
    // check that all closures are constants
    def toExp(scalaExp: c.Tree): Tree = scalaExp match {
      case Ident(term) => q"""_root_.com.github.johnynek.expression.Var[${b.tpe}](${term.toString})"""
    }

    val res = fn.tree match {
      // Here is a single arg function
      case Function(ValDef(_, termName, _, _) :: Nil, innerTree) =>
        val resExp = toExp(innerTree)
        val exp: Tree = q"""_root_.com.github.johnynek.expression.Lambda(
          _root_.com.github.johnynek.expression.Var[${a.tpe}](${termName.toString}), $resExp)"""
        c.Expr[ExpressionFn[A, B]](q"""new _root_.com.github.johnynek.expression.ExpressionFn[${a.tpe}, ${b.tpe}] {
         val expression = $exp
         val fn = ${fn.tree}
         def apply(a: ${a.tpe}) = fn.apply(a)
        }""")
      case _ => c.abort(fn.tree.pos, "Requires a function literal")
    }
    println(res)
    res
  }
}
