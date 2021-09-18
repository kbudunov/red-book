package com.kbudunov.redbook.funcdatastructure

object Exercise_3 extends App {

  val numbers = List(1, 2, 3, 4, 5, 6)

  val tail = List.tail(numbers)
  val addedHead = List.addHead(5, List(1, 2, 3))
  val dwResult = List.dropWhile[Int](numbers, (x: Int) => x > 2)
  val dResult = List.drop[Int](numbers, 2)
  val initResult = List.init2(numbers)
  println(initResult)

}

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  //Exercise_3_2
  def tail[A](numbers: List[A]): List[A] = {
    numbers match {
      case Nil           => numbers
      case Cons(_, tail) => tail
    }
  }

  private def headOption[A](elements: List[A]): Option[A] = {
    elements match {
      case Nil           => None
      case Cons(head, _) => Some(head)
    }
  }

  private val empty = List()

  private def reverse[A](elements: List[A]): List[A] = {
    @scala.annotation.tailrec
    def innerReverse[T](elements: List[T], acc: List[T]): List[T] = {
      elements match {
        case Nil              => acc
        case Cons(head, tail) => innerReverse(tail, Cons(head, acc))
      }
    }
    innerReverse(elements, List.empty)
  }

  //Exercise_3_3
  def addHead(headNumber: Int, numbers: List[Int]): List[Int] = {
    Cons(headNumber, numbers)
  }

  //Exercise_3_4
  def drop[A](elements: List[A], n: Int): List[A] = {

    @scala.annotation.tailrec
    def inner(elements: List[A], n: Int, acc: Int): List[A] = {
      elements match {
        case Nil                       => elements
        case Cons(_, tail) if acc == n => tail
        case Cons(_, tail)             => inner(tail, n, acc + 1)
      }
    }
    inner(elements, n, 1)
  }

  //Exercise_3_5
  def dropWhile[A](elements: List[A], f: A => Boolean): List[A] = {
    @scala.annotation.tailrec
    def inner(elements: List[A]): List[A] = {
      elements match {
        case Nil                      => elements
        case Cons(head, _) if f(head) => elements
        case Cons(_, tail)            => inner(tail)
      }
    }
    inner(elements)
  }

  //Exercise_3_6
  def init[A](elements: List[A]): List[A] = {

    @scala.annotation.tailrec
    def inner(elements: List[A], resultElements: List[A]): List[A] = {
      elements match {
        case Nil              => elements
        case Cons(_, Nil)     => resultElements
        case Cons(head, tail) => inner(tail, Cons(head, resultElements))
      }
    }

    List.reverse(inner(elements, List.empty))
  }

  //Exercise_3_6 much better
  def init2[A](elements: List[A]): List[A] = {
    List.reverse(List.tail(List.reverse(elements)))
  }

  //Exercise_3_7
}
