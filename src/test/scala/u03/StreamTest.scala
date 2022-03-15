package u03

import org.junit.Test
import org.junit.Assert.assertEquals

import Streams.*
import Lists.*
import List.*

class StreamTest:

  @Test def dropTest(): Unit =
    val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))),
      Stream.toList(Stream.drop(s)(6)))
    assertEquals(Stream.empty(), Stream.drop(s)(10))
    assertEquals(Stream.empty(), Stream.drop(Stream.empty())(10))
    assertEquals(s, Stream.drop(s)(0))