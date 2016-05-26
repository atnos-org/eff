package org.atnos.eff.syntax

import org.atnos.eff._, monix._
import scala.concurrent._, duration._
import _root_.monix.execution._
import _root_.monix.eval._
import cats.data._

import scala.concurrent.ExecutionContext

object monix extends monix

trait monix {

  implicit class TaskEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def awaitTask[U <: Effects](atMost: Duration)
                               (implicit member: Member.Aux[Task, R, U], ec: ExecutionContext, s: Scheduler): Eff[U, Throwable Xor A] =
      org.atnos.eff.monix.TaskEffect.awaitTask(e)(atMost)

  }

}
