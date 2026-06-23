package org.atnos.site
package lib

object WriterEffectPage extends UserGuidePage {
  def is = "Writer".title ^ s2"""

You can then define your own custom `Fold` to log the values to a file:${snippet {
      import org.atnos.eff._, all._, syntax.all.given
      import cats.data.Writer
      import java.io.PrintWriter
      import java.io.File
      import java.nio.file.Files
      import scala.io

      type S = Fx.fx1[Writer[String, *]]

      val action: Eff[S, Int] = for {
        a <- pure[S, Int](1)
        _ <- tell("first value " + a)
        b <- pure[S, Int](2)
        _ <- tell("second value " + b)

      } yield a + b

      // define a fold to output values
      def fileFold(path: File) = new RightFold[String, Unit] {
        type S = PrintWriter
        val init: S = new PrintWriter(path)

        def fold(a: String, s: S): S = { s.println(a); s }

        def finalize(s: S): Unit =
          s.close
      }

      val log = Files.createTempDirectory("").resolve("log").toFile
      action.runWriterFold(fileFold(log)).run
      try {
        io.Source.fromFile(log).getLines().toList
      } finally {
        log.delete()
      }
    }.eval}


"""
}
