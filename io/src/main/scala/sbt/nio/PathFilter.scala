package sbt.nio

import java.nio.file._

private[nio] trait PathFilter {
  def apply(path: Path): Boolean

  /** Constructs a filter that accepts a `Path` if it matches either this filter or the given `filter`. */
  def ||(filter: PathFilter): PathFilter = new OrFilter(this, filter)

  /** Constructs a filter that accepts a `Path` if it matches both this filter and the given `filter`. */
  def &&(filter: PathFilter): PathFilter = new AndFilter(this, filter)

  /** Constructs a filter that accepts a `Path` if it does not match this filter. */
  def unary_- : PathFilter = new NotFilter(this)
}
private[nio] object PathFilter {
  private[nio] final class FromFileFilter(private val filter: String => Boolean)
      extends PathNameFilter {
    override def apply(name: String): Boolean = filter(name)
    override def apply(path: Path): Boolean = apply(path.toString)
    override def equals(o: Any): Boolean = o match {
      case that: FromFileFilter => this.filter == that.filter
      case _                    => false
    }
    override def hashCode: Int = filter.hashCode
    override def toString: String = s"FullNameFilter($filter)"
  }
}
trait PathNameFilter extends PathFilter {
  def apply(name: String): Boolean
  override def apply(path: Path): Boolean = apply(path.getFileName.toString)

  /** Constructs a filter that accepts a `Path` if it matches either this filter or the given `filter`. */
  def |(filter: PathNameFilter): PathNameFilter = new OrNameFilter(this, filter)

  /** Constructs a filter that accepts a `Path` if it matches both this filter and the given `filter`. */
  def &(filter: PathNameFilter): PathNameFilter = new AndNameFilter(this, filter)
}

private[nio] abstract class AbstractAndFilter[T <: PathFilter](val left: T,
                                                               val right: T,
                                                               private[this] val sep: String) {
  override def equals(o: Any): Boolean = o match {
    case that: AbstractAndFilter[_] => this.left == that.left && this.right == that.right
    case _                          => false
  }
  override def hashCode: Int = this.left.hashCode ^ this.right.hashCode
  override def toString = s"$left $sep $right"
}
private[nio] final class AndFilter(private[this] val left: PathFilter,
                                   private[this] val right: PathFilter)
    extends AbstractAndFilter(left, right, "&&")
    with PathFilter {
  override def apply(path: Path): Boolean = left(path) && right(path)
}
private[nio] final class AndNameFilter(private[this] val left: PathNameFilter,
                                       private[this] val right: PathNameFilter)
    extends AbstractAndFilter(left, right, "&")
    with PathNameFilter {
  override def apply(name: String): Boolean = left(name) && right(name)
}
private[nio] final class NotFilter(val filter: PathFilter) extends PathNameFilter {
  override def apply(path: Path): Boolean = !filter(path)
  override def apply(name: String): Boolean = false
  override def equals(o: Any): Boolean = o match {
    case that: NotFilter => this.filter == that.filter
    case _               => false
  }
  override def hashCode: Int = filter.hashCode
  override def toString = s"!$filter"
}
private[nio] abstract class AbstractOrFilter[T <: PathFilter](val left: T,
                                                              val right: T,
                                                              private[this] val sep: String) {
  override def equals(o: Any): Boolean = o match {
    case that: AbstractOrFilter[T] => this.left == that.left && this.right == that.right
    case _                         => false
  }
  override def hashCode: Int = this.left.hashCode ^ this.right.hashCode
  override def toString = s"$left $sep $right"
}
private[nio] final class OrFilter(private[this] val left: PathFilter,
                                  private[this] val right: PathFilter)
    extends AbstractOrFilter(left, right, "||")
    with PathFilter {
  override def apply(path: Path): Boolean = left(path) || right(path)
}
private[nio] final class OrNameFilter(private[this] val left: PathNameFilter,
                                      private[this] val right: PathNameFilter)
    extends AbstractOrFilter(left, right, "&")
    with PathNameFilter {
  override def apply(name: String): Boolean = left(name) || right(name)
}
final class ExtensionFilter(val extensions: String*) extends PathNameFilter {
  private val set = extensions.toSet

  /** Returns `true` to include the `name`, `false` to exclude it. */
  override def apply(name: String): Boolean = {
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
  override def |(filter: PathNameFilter): PathNameFilter = filter match {
    case that: ExtensionFilter => new ExtensionFilter(this.extensions ++ that.extensions: _*)
    case _                     => super.|(filter)
  }
  override def toString: String = s"ExtensionFilter(${extensions mkString ","})"
}

/** A [[PathNameFilter]] that only accepts a single input file. */
final class ExactPathNameFilter(private[this] val path: Path) extends PathNameFilter {
  private val fileName = path.getFileName.toString
  override def apply(name: String): Boolean = name == fileName
  override def toString: String = s"ExactPathNameFilter($fileName)"
  override def equals(o: Any): Boolean = o match {
    case that: ExactPathNameFilter => this.fileName == that.fileName
    case _                         => false
  }
  override def hashCode: Int = fileName.hashCode
}

object AllPass extends PathNameFilter {
  override def apply(name: String): Boolean = true
  override def apply(path: Path): Boolean = true
  override def toString: String = "AllPass"
}
object NoPass extends PathNameFilter {
  override def apply(name: String): Boolean = false
  override def apply(path: Path): Boolean = false
  override def toString: String = "NoPass"
}
