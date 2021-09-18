package com.kbudunov.redbook.funcprogramming

object Exercise_2_5 extends App {

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

  val f: Int => Int = (a: Int) => a * 2
  val g: Int => Int = (a: Int) => a * 5

  private val intToInt: Int => Int = compose[Int, Int, Int](f, g)

  println(intToInt(5))

}
