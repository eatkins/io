package sbt.io

private[sbt] trait Alternative[A, B] {
  def |(g: A => Option[B]): A => Option[B]
}

sealed abstract class IOSyntax1 extends IOSyntax2 {
  def singleFileFinder(file: java.io.File): PathFinder = PathFinder(file)
  // This enables the legacy implicit implementation, but only if the user explicitly
  // imports `sbt.io.syntax.implicits.FileAsPathFinder`
  implicit def singleFileFinderWithEvidence(file: java.io.File)(
      implicit ev: syntax.FileAsPathFinder): PathFinder = {
    ev.unused() // Without this, we get a fatal error due to the unused parameter
    PathFinder(file)
  }
}
sealed trait IOSyntax2 {
  implicit def singleFileInputFinder(file: java.io.File): PathFinderInput =
    new PathFinderInput(file, new ExactFileFilter(file), recursive = false)
}

sealed abstract class IOSyntax0 extends IOSyntax1 {
  implicit def alternative[A, B](f: A => Option[B]): Alternative[A, B] = new Alternative[A, B] {
    def |(g: A => Option[B]) = a => f(a) orElse g(a)
  }
}

object syntax extends IOSyntax0 {
  type File = java.io.File
  type URI = java.net.URI
  type URL = java.net.URL

  def uri(s: String): URI = new URI(s)
  def file(s: String): File = new File(s)
  def url(s: String): URL = new URL(s)

  implicit def fileToRichFile(file: File): RichFile = new RichFile(file)
  implicit def filesToFinder(cc: Traversable[File]): PathFinder = PathFinder.strict(cc)
  sealed trait FileAsPathFinder { private[io] def unused(): Unit = () }
  object implicits {
    implicit case object FileAsPathFinder extends FileAsPathFinder
  }
}
