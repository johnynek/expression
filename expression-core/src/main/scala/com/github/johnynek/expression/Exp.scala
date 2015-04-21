package com.github.johnynek.expression

case class Scope(m: Map[Var[_], Exp[_]]) {
  def put[T](pair: (Var[T], Exp[T])): Scope = new Scope(m + pair)
  def apply[T](v: Var[T]): Either[String, Exp[T]] = m.get(v) match {
    case Some(exp) => Right(exp.asInstanceOf[Exp[T]])
    case None => Left(s"$v not found. Scope: $m")
  }
}
object Scope {
  def empty: Scope = Scope(Map.empty)
}

sealed trait Exp[A] {
  /**
   * Either evaluate the Expression, or return an error string
   * (about missing Vars is the only thing that can go wrong)
   */
  def run(scope: Scope): Either[String, A]
}

// TODO probably want only typed Const Expressions (Int, Long, Float, Double, String, Unit, Boolean)
case class Const[A](get: A) extends Exp[A] {
  def run(scope: Scope) = Right(get)
}

// this can either be done as a special Fn or here
// Since this makes Exp an Applicative Function, I leave it here
case class Pair[A, B](first: Exp[A], second: Exp[B]) extends Exp[(A, B)] {
  def run(scope: Scope) = for {
    a <- first.run(scope).right
    b <- second.run(scope).right
  } yield (a, b)
}

// This is Functor.map on Exp
case class Apply[B, A](in: Exp[B], fn: Exp[Fn[B, A]]) extends Exp[A] {
  def run(scope: Scope) = for {
    b <- in.run(scope).right
    f <- fn.run(scope).right
  } yield f.run(b)
}

case class Var[A](name: String) extends Exp[A] {
  def run(scope: Scope) = for {
    a <- scope(this).right
    result <- a.run(scope).right
  } yield result
}

case class Lambda[A, B](x: Var[A], result: Exp[B]) extends Exp[Fn[A, B]] {
  def run(scope: Scope) = Right(ExpFn.LambdaFn(this, scope))
}


object Exp {
  // would not be here in real life, as it is an escape hatch
  // instead, we would convert the Exp to code/source that can
  // be run elsewhere
  def run[A](e: Exp[A]): Either[String, A] = e.run(Scope.empty)

  implicit class FnExp[A, B](val e: Exp[Fn[A, B]]) extends AnyVal {
    def apply(a: Exp[A]): Exp[B] = Apply[A, B](a, e)
    def andThen[C](that: Exp[Fn[B, C]]): Exp[Fn[A, C]] = {
      val va = Var[A]("a")
      val eb = Apply(va, e)
      val ec = Apply(eb, that)
      Lambda(va, ec)
    }
    def zip[C](that: Exp[Fn[A, C]]): Exp[Fn[A, (B, C)]] = {
      val va = Var[A]("a")
      val b = Apply(va, e)
      val c = Apply(va, that)
      Lambda(va, Pair(b, c))
    }
  }
}

object ExpFn {
  import Fn._

  def fst[A, B](p: Exp[(A, B)]): Exp[A] =
    Apply(p, lift(Fst[A, B]()))
  def snd[A, B](p: Exp[(A, B)]): Exp[B] =
    Apply(p, lift(Snd[A, B]()))
  def addInt(a: Exp[Int], b: Exp[Int]): Exp[Int] =
    Apply(Pair(a, b), lift(AddInt))

  def ifThenElse[A](b: Exp[Boolean], iftrue: Exp[A], iffalse: Exp[A]): Exp[A] =
    Apply(Pair(b, Pair(iftrue, iffalse)), lift(IfThenElse[A]()))

  object Opt {
    def empty[A]: Exp[Option[A]] = Const(None)
    def apply[A](e: Exp[A]): Exp[Option[A]] = Apply(e, lift(ToOption[A]()))
    def flatMap[A, B](e: Exp[Option[A]], f: Exp[Fn[A, Option[B]]]): Exp[Option[B]] = {
      val onEmpty = Lambda(Var[Unit](""), empty[B])
      val onNonEmpty = f
      Apply(Pair(e, Pair(onEmpty, onNonEmpty)), lift(OptionFold[A, Option[B]]))
    }
    // In terms of the above
    def map[A, B](e: Exp[Option[A]], f: Exp[Fn[A, B]]): Exp[Option[B]] =
      flatMap(e, f.andThen {
        val x = Var[B]("b")
        Lambda(x, Opt(x))
      })
  }

  // non fundamental ops
  def lift[A, B](f: Fn[A, B]): Exp[Fn[A, B]] = Const(f)
  def swap[A, B](p: Exp[(A, B)]): Exp[(B, A)] =
    Pair(snd(p), fst(p))

  def curry[A, B, C](fn: Exp[Fn[(A, B), C]]): Exp[Fn[A, Fn[B, C]]] = {
    val a = Var[A]("A")
    val b = Var[B]("B")
    val pair = Pair(a, b)
    val c = Apply(pair, fn)
    val fnbc = Lambda(b, c)
    Lambda(a, fnbc)
  }
  def uncurry[A, B, C](fn: Exp[Fn[A, Fn[B, C]]]): Exp[Fn[(A, B), C]] = {
    val pair = Var[(A, B)]("pair")
    val a = fst(pair)
    val fnbc = Apply(a, fn)
    val b = snd(pair)
    val c = Apply(b, fnbc)
    Lambda(pair, c)
  }

  case class LambdaFn[A, B](l: Lambda[A, B], scope: Scope) extends Fn[A, B] {
    def run(a: A) = {
      val newScope = scope.put(l.x -> Const(a))
      val b = l.result.run(newScope)
      b.right.get
    }
  }
}

/**
 * Private Fn class, so we can't write general scala functions
 */
sealed trait Fn[A, B] {
  def run(a: A): B
}

object Fn {
  case class Fst[A, B]() extends Fn[(A, B), A] {
    def run(b: (A, B)) = b._1
  }
  case class Snd[A, B]() extends Fn[(A, B), B] {
    def run(b: (A, B)) = b._2
  }
  case object AddInt extends Fn[(Int, Int), Int] {
    def run(p: (Int, Int)) = p._1 + p._2
  }
  case class IfThenElse[A]() extends Fn[(Boolean, (A, A)), A] {
    def run(in: (Boolean, (A, A))) = if(in._1) in._2._1 else in._2._2
  }
  case class ToOption[A]() extends Fn[A, Option[A]] {
    def run(a: A) = Option(a)
  }
  case class OptionFold[A, B]() extends Fn[(Option[A], (Fn[Unit, B], Fn[A, B])), B] {
    def run(args: (Option[A], (Fn[Unit, B], Fn[A, B]))): B = {
      val (opt, (empty, nonempty)) = args
      opt match {
        case None => empty.run(())
        case Some(a) => nonempty.run(a)
      }
    }
  }
}

trait ExpressionFn[A, B] extends Function1[A, B] {
  def expression: Exp[Fn[A, B]]
}
