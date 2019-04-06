package sbt.nio

import java.nio.file.Path

package object syntax {
  import scala.language.dynamics

  case object **
  case object * extends Dynamic {
    def applyDynamic(extension: String): ExtensionFilter = new ExtensionFilter(extension)
  }
  implicit class PathOps(val path: Path) extends AnyVal {
    def /(star: **.type): Glob = Glob(path, (1, Int.MaxValue), AllPass)
    def /(star: *.type): Glob = Glob(path, (1, 1), AllPass)
  }
  implicit class GlobObs(val glob: Glob) extends AnyVal {
    def /(star: **.type): Glob =
      Glob(glob.base, (glob.range._1 + 1, Int.MaxValue), glob.filter)
    def /(star: *.type): Glob =
      Glob(glob.base, (glob.range._1 + 1, glob.range._2 + 1), glob.filter)
  }
}
