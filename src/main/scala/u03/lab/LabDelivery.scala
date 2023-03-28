package u03.lab

import u03.lab.LabDelivery.List

import scala.annotation.tailrec

object LabDelivery extends App :

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
    /*
     * TASK 1.1
     */
    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(_, t) if n > 0 => drop(t, n - 1)
      case l => l

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
    /*
     * TASK 1.2
     */
    import u02.Optionals.*
    import u02.Optionals.Option.*
    def max(l: List[Int]): Option[Int] = l match
      case Nil() => None()
      case Cons(h, t) => max(t) match
        case Some(m) if h < m => Some(m)
        case _ => Some(h)
    /*
     * TASK 2.4
     */
    @tailrec
    def foldLeft[A, B](l: List[A])(d: B)(acc: (B, A) => B): B = l match
      case Nil() => d
      case Cons(h, t) => foldLeft(t)(acc(d, h))(acc)

    def foldRight[A, B](l: List[A])(d: B)(acc: (A, B) => B): B = l match
      case Nil() => d
      case Cons(h, t) => acc(h, foldRight(t)(d)(acc))

    def reverse[A](l: List[A]): List[A] = foldLeft(l)(Nil())((a, b) => Cons(b, a))

    def foldRight2[A](l: List[A])(d: A)(acc: (A, A) => A): A = foldLeft(reverse(l))(d)((x, y) => acc(y, x))
  /*
   * TASK 2.3
   */
  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:
    import List.*
    def name(p: Person): String = p match
      case Student(n, _) => n
      case Teacher(n, _) => n

    def isStudent(p: Person): Boolean = p match
      case Student(_, _) => true
      case _ => false

    def getCourses(pl: List[Person]): List[String] = flatMap(pl)(p => p match
      case Teacher(_, course) => Cons(course, Nil())
      case _ => Nil()
    )

  /*
   * TASK 3
   */
  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:
    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): List[A] = stream match
      case Cons(h, t) => List.Cons(h(), toList(t()))
      case _ => List.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), filter(tail())(pred))
      case Cons(_, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = stream match
      case Cons(head, tail) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))
    /*
     * TASK 3.5
     */
    @tailrec
    def drop[A](stream: Stream[A])(n: Int): Stream[A] = stream match
      case Cons(_, tail) if n > 0 => drop(tail())(n - 1)
      case s => s
    /*
     * TASK 3.6
     */
    def constant[A](e: A): Stream[A] = iterate(e)(e => e)
    /*
     * TASK 3.7
     */
    def fibonacci: Stream[Int] = map(iterate((0, 1))((a, b) => (b, a + b)))((a, _) => a)

    def append[A](stream1: Stream[A], stream2: Stream[A]): Stream[A] = stream1 match
      case Cons(h, t) => cons(h(), append(t(), stream2))
      case _ => stream2

    def flatMap[A, B](stream: Stream[A])(f: A => Stream[B]): Stream[B] = stream match
      case Cons(h, t) => append(f(h()), flatMap(t())(f))
      case _ => Empty()
