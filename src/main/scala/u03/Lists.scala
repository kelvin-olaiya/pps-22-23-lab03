package u03

import scala.annotation.tailrec

object Lists extends App :

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

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
      case (l, 0) => l
      case (Nil(), _) => Nil()
      case (Cons(_, t), n) => drop(t, n - 1)

    def append[A](l1: List[A], l2: List[A]): List[A] = l1 match
      case Nil() => l2
      case Cons(h, t) => Cons(h, append(t, l2))

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case _ => Nil()

    def map2[A, B](l: List[A])(f: A => B): List[B] = flatMap(l)(v => Cons(f(v), Nil()))

    def filter2[A](l: List[A])(p: A => Boolean): List[A] = flatMap(l)(v => v match
      case e if p(e) => Cons(e, Nil())
      case _ => Nil()
    )

    import u02.Optionals.*
    import u02.Optionals.Option.*
    def max(l: List[Int]): Option[Int] = l match
      case Nil() => None()
      case Cons(h, t) => max(t) match
        case Some(v) if h < v => Some(v)
        case _ => Some(h)




