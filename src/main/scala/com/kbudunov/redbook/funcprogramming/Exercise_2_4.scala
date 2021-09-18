package com.kbudunov.redbook.funcprogramming

object Exercise_2_4 extends App {

  val g = curry[Int, Int, Int]((x, y) => x + y)
  println(g(2)(2))

  def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

}
