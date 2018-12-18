package restrictions.utils

import cats._
import cats.implicits._
import play.api.Logger
import restrictions.{SourceInfoMacros, SourceLocation}

import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}

object LogWhenEmpty {
  implicit class LogWhenEmptyOps[A, F[_]: Foldable](val f: F[A])(implicit logger: Logger) {
    def logWhenEmpty(extra: String): F[A] = macro SourceInfoMacros.generateLogWithPrefix[F[A]]
    def logWhenEmpty(name: String, extra: String)(implicit loc: SourceLocation): F[A] = {
      if (Foldable[F].isEmpty(f)) { logger.warn(s"🗑 [$name:${loc.line}] is empty ($extra)") }
      f
    }
  }
}

