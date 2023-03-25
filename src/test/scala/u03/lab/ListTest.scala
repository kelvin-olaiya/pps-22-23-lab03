package u03.lab

import org.junit.*
import org.junit.Assert.*
import u03.lab.LabDelivery.*
import List.*

object Definitions:
  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

import Definitions.*

class ListTest:
  @Test def testSum(): Unit =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_+1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_+""))

  @Test def testFilter(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_>=20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_!=20))

class TestTask1:
  @Test def testDrop(): Unit =
    assertEquals(Cons (20 , Cons (30 , Nil ())), drop(l, 1))
    assertEquals(Cons (30 , Nil ()), drop(l, 2))
    assertEquals(Nil(), drop(l, 5))

  @Test def testAppend(): Unit =
    val tail = Cons(40, Nil())
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(l, tail))
    
  @Test def testFlatMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def testMap2(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map2(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map2(l)(_ + ""))

  @Test def testFilter2(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filter2(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter2(l)(_ != 20))

  @Test def testMax(): Unit =
    import u02.Optionals.Option.*
    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(None(), max(Nil()))

class TestPearson:
  import Person.*
  val persons: List[Person] = Cons(Student("Kelvin", 2000),
    Cons(Teacher("Viroli", "PPS"),
      Cons(Student("Mario", 2002),
        Cons(Teacher("Ricci", "PCD"), Nil())
      )))
  @Test def testGetCourses(): Unit =
    assertEquals(Cons("PPS", Cons("PCD", Nil())), getCourses(persons))

class TestTask2:
  @Test def testReverse(): Unit =
    val lst = Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))
    assertEquals(Cons(4, Cons(3, Cons(2, Cons(1, Nil())))), reverse(lst))

  @Test def testFold(): Unit =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))
    assertEquals(-8, foldRight(lst)(0)(_ - _))
    assertEquals(-8, foldRight2(lst)(0)(_ - _))

class StreamsTest:
  import Stream.*
  val s = take(iterate(0)(_ + 1))(10)
  @Test def testDrop(): Unit =
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))), toList(drop(s)(6)))

  @Test def testConstant(): Unit =
    assertEquals(Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil()))))),  toList(take(constant("x"))(5)))

  @Test def testAppend(): Unit =
    assertEquals(
      Cons(1, Cons(2, Cons(3, Cons(4, Nil())))),
      toList(append(cons(1, cons(2, empty())), cons(3, cons(4, empty()))))
    )

  @Test def testFlatMap(): Unit =
    val s = cons(1, cons(2, cons(3, empty())))
    assertEquals(
      Cons(10, Cons(15, Cons(20, Cons(25, Cons(30, Cons(35, Nil())))))),
      toList(flatMap(s)(e => cons(e * 10, cons(e*10 + 5, empty()))))
    )

  @Test def testFibonacci(): Unit =
    assertEquals(
      Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Cons(21, Nil())))))))),
      toList(flatMap(take(iterate((0, 1))((a, b) => (b, a + b)))(8))((_, b) => cons(b, empty()))))
