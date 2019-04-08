package sbt.nio

import java.nio.file.Path

package object syntax {
  import scala.language.dynamics

  case object **
  case object * extends Dynamic {
    def selectDynamic(extension: String): ExtensionFilter = new ExtensionFilter(extension)
  }
  implicit class PathOps(val path: Path) extends AnyVal {
    def /(star: **.type): Glob = Glob(path, (1, Int.MaxValue), AllPass)
    def /(star: *.type): Glob = Glob(path, (1, 1), AllPass)
    def /(pathNameFilter: PathNameFilter): Glob =
      Glob(path, (1, 1), filter = pathNameFilter)
  }
  implicit class GlobObs(val glob: Glob) extends AnyVal {
    private def addLevel: (Int, Int) = glob.range match {
      case (l, Int.MaxValue)                              => (l, Int.MaxValue)
      case (l, u) if l < Int.MaxValue && u < Int.MaxValue => (l + 1, u + 1)
      case (l, u) if l < Int.MaxValue                     => (l + 1, u)
      case _                                              => (Int.MaxValue, Int.MaxValue)
    }
    private def increment(i: Int): Int = if (i < Int.MaxValue) i + 1 else Int.MaxValue
    def /(star: **.type): Glob = {
      Glob(glob.base, (increment(glob.range._1), Int.MaxValue), glob.filter)
    }
    def /(star: *.type): Glob =
      Glob(glob.base, addLevel, glob.filter)
    def /(pathNameFilter: PathNameFilter): Glob =
      Glob(glob.base, addLevel, filter = pathNameFilter)
  }
}
