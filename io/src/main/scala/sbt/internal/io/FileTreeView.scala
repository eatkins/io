package sbt.internal.io

import java.nio.file.{ Path => NioPath }

import sbt.io.{ FileAttributes, Glob }

/**
 * Provides a view into the file system that allows retrieval of the children of a particular path.
 * Specific implementations may or may not use a cache for retrieval.
 * @tparam T the type of object returned for each file
 */
trait FileTreeView[+T] extends AutoCloseable {

  /**
   * List the contents of the current directory.
   *
   * @param glob the files to include
   * @return a sequence of values corresponding to each path described by the glob
   */
  def list(glob: Glob, filter: T => Boolean): Seq[T]

  // Many, if not most, FileTreeViews should not create new resources.
  override def close(): Unit = {}
}
private[sbt] object FileTreeView {
  private[sbt] type Nio[T] = FileTreeView[(NioPath, T)]
  private[sbt] type Io[T] = FileTreeView[(java.io.File, T)]
  private[sbt] object AllPass extends (Any => Boolean) {
    override def apply(any: Any): Boolean = true
    override def toString: String = "AllPass"
  }
  private[sbt] val DEFAULT_NIO: Nio[FileAttributes] = DefaultFileTreeView
  private[sbt] val DEFAULT_IO: Io[FileAttributes] = new MappedFileTreeView(DefaultFileTreeView, {
    case (p: NioPath, a: FileAttributes) => p.toFile -> a
  }: ((NioPath, FileAttributes)) => (java.io.File, FileAttributes), closeUnderlying = true)
  private class MappedFileTreeView[+T, +R](view: FileTreeView[T],
                                           converter: T => R,
                                           closeUnderlying: Boolean)
      extends FileTreeView[R] {
    override def list(glob: Glob, filter: R => Boolean): Seq[R] = {
      view.list(glob, AllPass).flatMap { t =>
        val r: R = converter(t)
        if (filter(r)) r :: Nil else Nil
      }
    }
    override def close(): Unit = if (closeUnderlying) view.close()
  }
  private[sbt] implicit class NioFileTreeViewOps[T](val view: FileTreeView.Nio[T]) {
    def map[A >: T, B](f: (NioPath, A) => B): NioFileTreeView[B] = {
      val mapped: FileTreeView[(NioPath, B)] = {
        val converter: ((NioPath, A)) => (NioPath, B) = {
          case (path: NioPath, attrs) => path -> f(path, attrs)
        }
        new MappedFileTreeView(view, converter, closeUnderlying = true)
      }
      (glob: Glob, filter: ((NioPath, B)) => Boolean) =>
        mapped.list(glob, filter)
    }
    def flatMap[B, A >: T](f: (NioPath, A) => Traversable[B]): NioFileTreeView[B] = {
      val converter: ((NioPath, A)) => Traversable[(NioPath, B)] = {
        case (path: NioPath, attrs) => f(path, attrs).map(path -> _)
      }
      (glob: Glob, filter: ((NioPath, B)) => Boolean) =>
        view.list(glob, AllPass).flatMap(converter(_).filter(filter))
    }
  }
}
