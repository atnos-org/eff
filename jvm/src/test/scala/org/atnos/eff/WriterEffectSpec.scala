package org.atnos.eff

import cats.data._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.specs2.Specification

import scala.collection.mutable.ListBuffer

class WriterEffectSpec extends Specification { def is = s2"""

 A writer effect can use a side-effecting fold to be evaluated $sideEffecting

"""

  def sideEffecting = {
    type R[A] = Writer[String, A]
    type S = R |: NoEffect

    val action: Eff[S, String] = for {
      f <- tell[S, String]("hello")
      h <- tell[S, String]("world")
    } yield f+" "+h

    val messages: ListBuffer[String] = new ListBuffer[String]

    action.runWriterUnsafeFold(UnsafeFold((m: String) => messages.append(m)))

    messages.toList ==== List("hello", "world")

  }


}

