package org.atnos.eff

import org.specs2.Specification

trait Specs2Compat { self: Specification =>

  implicit final class MustEqualExtension[A](a1: A) {
    def ====(a2: A) = a1 must beEqualTo(a2)
    def must_==(a2: A) = a1 must beEqualTo(a2)
  }

}
