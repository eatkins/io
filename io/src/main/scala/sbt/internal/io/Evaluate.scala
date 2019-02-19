package sbt.internal.io
import scala.util.control.NonFatal

private[io] object Evaluate {
  def apply[T](t: => T): Either[Throwable, T] =
    try Right(t)
    catch { case NonFatal(t) => Left(t) }
}
