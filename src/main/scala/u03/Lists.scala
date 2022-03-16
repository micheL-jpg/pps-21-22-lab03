package u03

import u02.Modules.Person
import u02.Modules.Person.*

import scala.annotation.tailrec
import u02.Optionals.Option
import u02.Optionals.Option.*


object Lists extends App:

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def mapFlat[A, B](l: List[A])(mapper: A => B): List[B] =
      flatMap(l)(v => Cons(mapper(v), Nil()))

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    def filterFlat[A](l1: List[A])(pred: A => Boolean): List[A] =
      flatMap(l1)(h => pred(h) match {
        case true => Cons(h, Nil())
        case false => Nil()
      })

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(head, tail) if n == 0 => l
      case Cons(head, tail) if n > 1 => drop(tail, n-1)
      case Cons(head, tail) => tail
      case _ => Nil()

    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Cons(head, tail) => Cons(head, append(tail, right))
      case Nil() => right

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(head, tail) => append(f(head), flatMap(tail)(f))
      case Nil() => Nil()

    def max(l: List[Int]): Option[Int] =
      @tailrec
      def _max(l: List[Int], m: Option[Int]): Option[Int] = l match
        case Cons(h, t) if h > orElse(m, 0) => _max(t, Some(h))
        case Cons(h, t) => _max(t, m)
        case Nil() if isEmpty(m) => None()
        case _ => m
        
      _max(l, None())

    def getCourses(l: List[Person]): List[String] =
      flatMap(l)(v => v match
        case Student(_, _) => Nil()
        case Teacher(_, c) => Cons(c, Nil())
      )

    @tailrec
    def foldLeft[A, B](l: List[A])(start: B)(f: (B, A) => B): B = l match
      case Cons(head, tail) => foldLeft(tail)(f(start, head))(f)
      case Nil() => start

    def foldRight[A, B](l: List[A])(start: B)(f: (A, B) => B): B = l match
      case Cons(head, tail) => f(head, foldRight(tail)(start)(f))
      case Nil() => start
      
  end List

  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
