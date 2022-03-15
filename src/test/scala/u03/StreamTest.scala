package u03

import org.junit.Test
import org.junit.Assert.assertEquals

import Streams.*
import Lists.*
import List.*

class StreamTest:

  @Test def testDrop(): Unit =
    val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))),
      Stream.toList(Stream.drop(s)(6)))
    assertEquals(Stream.empty(), Stream.drop(s)(10))
    assertEquals(Stream.empty(), Stream.drop(Stream.empty())(10))
    assertEquals(s, Stream.drop(s)(0))

  @Test def testConstant(): Unit =
    assertEquals(Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil ()))))),
      Stream.toList(Stream.take(Stream.constant("x"))(5)))

  @Test def testFibonacci(): Unit =

    val fibs = Stream.map(Stream.iterate(0, 1)((a, b) => (a + b, a)))((a, b) => a)

    assertEquals(Cons (0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5,
      Cons(8, Cons(13 , Nil())))))))),
      Stream.toList(Stream.take(fibs)(8)))