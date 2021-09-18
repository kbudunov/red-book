package com.kbudunov.redbook.funcprogramming

object Exercise_2_1 extends App {

  println(fibonacci(10))

  def factorial(x: Int): Int = {
    def go(x: Int): Int = {
      x match {
        case x if x <= 1 => x
        case _           => x * go(x - 1)
      }
    }
    go(x)
  }

  def fibonacci(x: Int): Int = {
    def go(n: Int): Int = {
      if (n <= 1) n
      else go(n - 1) + go(n - 2)
    }
    go(x)
  }

}
