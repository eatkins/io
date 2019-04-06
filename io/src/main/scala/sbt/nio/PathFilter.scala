package sbt.nio

import java.nio.file._

sealed trait PathFilter extends (Path => Boolean) {

  /** Constructs a filter that accepts a `Path` if it matches either this filter or the given `filter`. */
  def ||(filter: Path => Boolean): PathFilter = new OrFilter(this, filter)

  /** Constructs a filter that accepts a `Path` if it matches both this filter and the given `filter`. */
  def &&(filter: Path => Boolean): PathFilter = new AndFilter(this, filter)

  /** Constructs a filter that accepts a `Path` if it does not match this filter. */
  def unary_- : PathFilter = new NotFilter(this)
}
object PathFilter {
  private class impl(val pathFilter: Path => Boolean) extends PathFilter {
    override def apply(path: Path): Boolean = pathFilter(path)
    override def equals(o: Any): Boolean = o match {
      case that: impl => this.pathFilter == that.pathFilter
      case _          => false
    }
    override def hashCode: Int = pathFilter.hashCode
    override def toString: String = s"PathFilter($pathFilter)"
  }
  implicit def default(filter: Path => Boolean): PathFilter = new impl(filter)
}

private[sbt] final class AndFilter(val left: PathFilter, val right: Path => Boolean)
    extends PathFilter {
  override def apply(path: Path): Boolean = left(path) && right(path)
  override def equals(o: Any): Boolean = o match {
    case that: AndFilter => this.left == that.left && this.right == that.right
    case _               => false
  }
  override def hashCode: Int = this.left.hashCode ^ this.right.hashCode
  override def toString = s"$left && $right"
}
private[sbt] final class NotFilter(filter: Path => Boolean) extends PathFilter {
  override def apply(path: Path): Boolean = !filter(path)
  override def equals(o: Any): Boolean = o match {
    case that: NotFilter => this.filter == that.filter
    case _               => false
  }
  override def hashCode: Int = filter.hashCode
  override def toString = s"!$filter"
}
private[sbt] final class OrFilter(val left: Path => Boolean, val right: Path => Boolean)
    extends PathFilter {
  override def apply(path: Path): Boolean = left(path) || right(path)
  override def equals(o: Any): Boolean = o match {
    case that: AndFilter => this.left == that.left && this.right == that.right
    case _               => false
  }
  override def hashCode: Int = this.left.hashCode ^ this.right.hashCode
  override def toString = s"$left || $right"
}
final class ExtensionFilter(val extensions: String*) extends PathFilter {
  private val set = extensions.toSet

  /** Returns `true` to include the `name`, `false` to exclude it. */
  override def apply(path: Path): Boolean = {
    val name = path.getFileName.toString
    val extension = name.lastIndexOf('.') match {
      case l if l >= 0 && l < name.length => name.substring(l + 1)
      case _                              => ""
    }
    set.contains(extension)
  }

  override def equals(o: Any): Boolean = o match {
    case that: ExtensionFilter => this.set == that.set
    case _                     => false
  }
  override lazy val hashCode: Int = extensions.hashCode

  /** Constructs a filter that accepts a `Path` if it matches either this filter or the given `filter`. */
  override def ||(filter: Path => Boolean): PathFilter = filter match {
    case that: ExtensionFilter => new ExtensionFilter(this.extensions ++ that.extensions: _*)
    case _                     => super.||(filter)
  }
  override def toString: String = s"ExtensionFilter(${extensions mkString ","})"
}

/** A [[PathFilter]] that only accepts a single input file. */
final class ExactPathFilter(val path: Path) extends PathFilter {
  override def accept(f: Path): Boolean = f == file
  override def toString: String = s"ExactFileFilter($file)"
  override def equals(o: Any): Boolean = o match {
    case that: ExactPathFilter => this.path == that.path
    case _                     => false
  }
  override def hashCode: Int = path.hashCode
}
