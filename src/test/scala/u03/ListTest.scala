package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*
import u02.Modules.Person.*
import u02.Optionals.Option.*

class ListTest:
  import List.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testMapFlat(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), mapFlat(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), mapFlat(l)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  @Test def testFilterFlat(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filterFlat(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filterFlat(l)(_ != 20))

  @Test def testDrop(): Unit =
    assertEquals(Cons (20 , Cons (30 , Nil ())), drop(l, 1))
    assertEquals(Cons (30 , Nil ()), drop(l, 2))
    assertEquals(Nil(), drop(l, 5))
    assertEquals(l, drop(l, 0))

  @Test def testAppend(): Unit =
    val tail = Cons(40, Nil())
    assertEquals(Cons (10 , Cons (20 , Cons (30 , Cons (40 , Nil ())))),
      append(l, tail))
    assertEquals(tail, append(Nil(), tail))

  @Test def testFlatMap(): Unit =
    assertEquals(Cons (11 , Cons (21 , Cons (31 , Nil ()))),
      flatMap(l)(v => Cons (v + 1, Nil ())))

  @Test def maxTest(): Unit =
    assertEquals(Some(30), max(l))
    assertEquals(None(), max (Nil ()))
    assertEquals(Some(25), max ( Cons (10 , Cons (25 , Cons (20 , Nil ())))))

  @Test def testCourses(): Unit =
    val list = Cons(Teacher("Viroli", "pps"), Cons(Teacher("Ricci", "pcd"),
      Cons(Student("Bachetti", 1998), Nil())))
    assertEquals(Cons("pps", Cons("pcd", Nil())), getCourses(list))
    val list2 = drop(list, 2)
    assertEquals(Nil(), getCourses(list2))