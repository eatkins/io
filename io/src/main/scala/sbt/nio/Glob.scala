/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.nio

import java.io.File
import java.nio.file._
import java.util

import sbt.io.{ ExactFileFilter, FileFilter, PathFinder, SimpleFileFilter }
import sbt.nio.PathNameFilter.WrappedPathFilter

import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder

/**
 * Represents a filtered subtree of the file system.
 */
sealed trait Glob {

  /**
   * The root of the file system subtree.
   */
  def base: Path

  /**
   * Controls which paths should be considered part of the glob based on the number of components
   * in the path when it is relativized with respect to the base path. The boundaries are inclusive.
   * A range of `(0, 0)` implies that only the base path is accepted. A range of `(0, 1)` implies
   * that the base path and its immediate children are accepted but no children of subdirectories
   * are included. A range of `(1, Int.MaxValue)` implies that all children of the base path are
   * accepted and so on.
   *
   * @return the range of relative path name lengths to accepts.
   */
  def range: (Int, Int)

  /**
   * The filter to apply to elements found in the file system subtree.
   * @return the filter.
   */
  def filter: PathFilter

}
private[sbt] sealed trait GlobBuilder[G] extends Any {
  def /(component: String): G
  def \(component: String): G
  def glob(filter: FileFilter): G
  def *(filter: FileFilter): G
  def globRecursive(filter: FileFilter): G
  def allPaths: G
  def **(filter: FileFilter): G
}
private[sbt] sealed trait ToGlob extends Any {
  def toGlob: Glob
}
object Glob {
  private[this] class ConvertedFileFilter(val f: FileFilter) extends PathNameFilter {
    override def apply(path: Path): Boolean = f.accept(path.toFile)
    override def apply(name: String): Boolean = false
    override def equals(o: Any): Boolean = o match {
      case that: ConvertedFileFilter => this.f == that.f
      case _                         => false
    }
    override def hashCode: Int = f.hashCode
    override def toString: String = s"ConvertedFileFilter($f)"
  }
  private[this] object ConvertedFileFilter {
    def apply(fileFilter: FileFilter): PathFilter = fileFilter match {
      case sbt.io.AllPassFilter       => AllPass
      case sbt.io.NothingFilter       => NoPass
      case af: sbt.io.AndFilter       => new AndFilter(apply(af.left), apply(af.right))
      case of: sbt.io.OrFilter        => new OrFilter(apply(of.left), apply(of.right))
      case nf: sbt.io.NotFilter       => new NotFilter(apply(nf.fileFilter))
      case ef: sbt.io.ExtensionFilter => new ExtensionFilter(ef.extensions: _*)
      case ef: ExactFileFilter        => new ExactPathNameFilter(ef.file.toPath)
      case filter                     => new ConvertedFileFilter(filter)
    }
  }
  private implicit class PathOps(val p: Path) extends AnyVal {
    def abs: Path = if (p.isAbsolute) p else p.toAbsolutePath
  }
  def apply(base: Path): Glob = new GlobImpl(base.abs, (0, 0), new ExactPathNameFilter(base))
  def apply(base: Path, range: (Int, Int), filter: PathFilter): Glob =
    new GlobImpl(base.abs, range, filter)
  private[sbt] def apply(base: Path, range: (Int, Int), filter: FileFilter): Glob =
    new GlobImpl(base.abs, range, ConvertedFileFilter(filter))
  private[nio] class GlobImpl(val base: Path, val range: (Int, Int), val nameFilter: PathNameFilter)
      extends Glob {
    private[this] val rf = new RangeFilter(base.abs, range)
    override def toString: String =
      s"Glob(\n  base = $base,\n  filter = $rf && $nameFilter,\n  depth = $range\n)"
    override def equals(o: Any): Boolean = o match {
      case that: GlobImpl =>
        this.base == that.base && this.range == that.range && this.nameFilter == that.nameFilter
      case _ => false
    }
    override def hashCode: Int = (((base.hashCode * 31) ^ filter.hashCode) * 31) ^ range.hashCode

    /**
     * The filter to apply to elements found in the file system subtree.
     *
     * @return the filter.
     */
    override def filter: PathNameFilter =
      new WrappedPathFilter((path: Path) => rf(path) && nameFilter(path))
  }
  private[sbt] trait Builder[T] extends Any with GlobBuilder[Glob] with ToGlob {
    def repr: T
    def converter: T => Path
    def /(component: String): Glob = {
      val base = converter(repr).resolve(component)
      Glob(base, (0, 0), (_: Path).getFileName.toString == component)
    }
    def \(component: String): Glob = this / component
    def glob(filter: FileFilter): Glob =
      Glob(converter(repr), (0, 1), ConvertedFileFilter(filter))
    def *(filter: FileFilter): Glob = glob(filter)
    def globRecursive(filter: FileFilter): Glob =
      Glob(converter(repr), (0, Int.MaxValue), ConvertedFileFilter(filter))
    def allPaths: Glob = globRecursive(sbt.io.AllPassFilter)
    def **(filter: FileFilter): Glob = globRecursive(filter)
    def toGlob: Glob = {
      val base = converter(repr)
      Glob(base, (0, 0), new ExactPathNameFilter(base))
    }
  }
  private[sbt] final class FileBuilder(val file: File) extends AnyVal with Builder[File] {
    override def repr: File = file
    override def converter: File => Path = (_: File).toPath
  }
  private[sbt] final class PathBuilder(val path: Path) extends AnyVal with Builder[Path] {
    override def repr: Path = path
    override def converter: Path => Path = identity
  }
  private[sbt] implicit class GlobOps(val glob: Glob) extends AnyVal {
    private[sbt] def toFileFilter: sbt.io.FileFilter =
      new SimpleFileFilter(file => glob.filter(file.toPath))
  }
  implicit def toPathFinder(glob: Glob): PathFinder = new PathFinder.GlobPathFinder(glob)
  implicit object ordering extends Ordering[Glob] {
    override def compare(left: Glob, right: Glob): Int = right.base.compareTo(left.base) match {
      // We want greater depth to come first because when we are using a Seq[Glob] to
      // register with the file system cache, it is more efficient to register the broadest glob
      // first so that we don't have to list the base directory multiple times.
      case 0 => right.range._2.compareTo(left.range._2)
      case i => i
    }
  }
  private[sbt] def all(globs: Traversable[Glob],
                       view: FileTreeView.Nio[FileAttributes]): Seq[(Path, FileAttributes)] =
    all(globs, view, (_, _) => true)
  private[sbt] def all(globs: Traversable[Glob],
                       view: FileTreeView.Nio[FileAttributes],
                       filter: (Path, FileAttributes) => Boolean): Seq[(Path, FileAttributes)] = {
    val sorted = globs.toSeq.sorted
    val simpleGlobs = sorted.map(g => Glob(g.base, (0, g.range._2), AllPass))
    def accept(path: Path): Boolean = simpleGlobs.exists(_.filter(path.resolve("a")))
    val visited = new util.HashSet[Path]
    val totalFilter: (Path, FileAttributes) => Boolean = {
      val pathFilter = (path: Path) => sorted.exists(_.filter(path))
      (path, attributes) =>
        pathFilter(path) && filter(path, attributes)
    }
    val result = new VectorBuilder[(Path, FileAttributes)]
    val maybeAdd: ((Path, FileAttributes)) => Unit = {
      case pair @ (path, attributes) =>
        if (totalFilter(path, attributes)) result += pair
    }
    sorted.foreach { glob =>
      val queue = new util.LinkedList[Path]
      queue.add(glob.base)
      @tailrec
      def impl(): Unit = {
        queue.poll match {
          case null =>
          case path if !visited.contains(path) =>
            visited.add(path)
            view.list(Glob(path, (1, 1), AllPass)) foreach {
              case pair @ (p, attributes) if attributes.isDirectory =>
                if (accept(p)) queue.add(p)
                maybeAdd(pair)
              case pair => maybeAdd(pair)
            }
            impl()
          case _ =>
        }
      }
      impl()
    }
    result.result()
  }

  private class RangeFilter(val base: Path, val range: (Int, Int)) extends PathNameFilter {
    override def apply(name: String): Boolean = false
    override def apply(path: Path): Boolean = {
      val globPath = base
      if (path.startsWith(globPath)) {
        if (path == globPath) {
          range._1 <= 0
        } else {
          val nameCount = globPath.relativize(path).getNameCount
          nameCount >= range._1 && nameCount <= range._2
        }
      } else {
        false
      }
    }
    override def toString: String = s"RangeFilter(basePath = $base, range = $range)"
    override def equals(o: Any): Boolean = o match {
      case that: RangeFilter => this.base == that.base && this.range == that.range
      case _                 => false
    }
    override def hashCode: Int = (base.hashCode * 31) ^ range.hashCode
  }
}
