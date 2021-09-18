package com.kbudunov.redbook.funcprogramming

object Exercise_2_2 extends App {

  val test = Array(2, 2, 3, 2)
  println(test.length)
  println(isSorted[Int](test, (x, y) => x <= y))

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @scala.annotation.tailrec
    def go(n: Int): Boolean = {
      n match {
        case n if n == as.length - 1        => true
        case n if ordered(as(n), as(n + 1)) => go(n + 1)
        case _                              => false
      }
    }
    go(0)
  }

}
