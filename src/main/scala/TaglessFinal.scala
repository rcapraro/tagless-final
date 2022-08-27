package com.capraro

object TaglessFinal {

  trait Expr[A] {
    val value: A
  }

  def b(boolean: Boolean): Expr[Boolean] = new Expr[Boolean] {
    override val value: Boolean = boolean
  }

  def i(int: Int): Expr[Int] = new Expr[Int] {
    override val value: Int = int
  }

  def or(left: Expr[Boolean], right: Expr[Boolean]): Expr[Boolean] = new Expr[Boolean] {
    override val value: Boolean = left.value || right.value
  }

  def and(left: Expr[Boolean], right: Expr[Boolean]): Expr[Boolean] = new Expr[Boolean] {
    override val value: Boolean = left.value && right.value
  }

  def not(expr: Expr[Boolean]): Expr[Boolean] = new Expr[Boolean] {
    override val value: Boolean = !expr.value
  }

  def sum(left: Expr[Int], right: Expr[Int]): Expr[Int] = new Expr[Int] {
    override val value: Int = left.value + right.value
  }

  def eval[A](expr: Expr[A]): A = expr.value
}

object TaglessFinal_v2 {
  trait Algebra[E[_]] {
    def b(boolean: Boolean): E[Boolean]

    def i(int: Int): E[Int]

    def or(left: E[Boolean], right: E[Boolean]): E[Boolean]

    def and(left: E[Boolean], right: E[Boolean]): E[Boolean]

    def sum(left: E[Int], right: E[Int]): E[Int]
  }

  case class SimpleExpr[A](value: A)

  given simpleAlgebra: Algebra[SimpleExpr] with {
    override def b(boolean: Boolean): SimpleExpr[Boolean] = SimpleExpr(boolean)

    override def i(int: Int): SimpleExpr[Int] = SimpleExpr(int)

    override def or(left: SimpleExpr[Boolean], right: SimpleExpr[Boolean]): SimpleExpr[Boolean] = SimpleExpr(left.value || right.value)

    override def and(left: SimpleExpr[Boolean], right: SimpleExpr[Boolean]): SimpleExpr[Boolean] = SimpleExpr(left.value && right.value)

    override def sum(left: SimpleExpr[Int], right: SimpleExpr[Int]): SimpleExpr[Int] = SimpleExpr(left.value + right.value)
  }

  def program1[E[_]](using alg: Algebra[E]): E[Boolean] = {
    import alg._
    or(b(true), and(b(true), b(false)))
  }

  def program2[E[_]](using alg: Algebra[E]): E[Int] = {
    import alg._
    sum(i(24), i(-3))
  }
}

def demoTaglessFinal(): Unit = {
  import TaglessFinal.*
  println(eval(or(b(true), and(b(true), b(false)))))
  println(eval(sum(i(24), i(-3))))
}

def demoTaglessFinal_v2(): Unit = {
  import TaglessFinal_v2.*
  println(program1[SimpleExpr].value)
  println(program2[SimpleExpr].value)
}

@main
def test(): Unit = {
  // demoTaglessFinal()
  demoTaglessFinal_v2()
}
