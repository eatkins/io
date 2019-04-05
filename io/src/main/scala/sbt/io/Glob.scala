/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.io

import java.io.File
import java.nio.file.{ Path => NioPath }

import sbt.io

/**
 * Represents a filtered subtree of the file system.
 */
trait Glob {

  /**
   * The root of the file system subtree.
   */
  def base: NioPath

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
  def filter: NioPath => Boolean

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
  private[this] implicit class ConvertedFileFilter(val f: FileFilter) extends (NioPath => Boolean) {
    override def apply(path: NioPath): Boolean = f.accept(path.toFile)
    override def equals(o: Any): Boolean = o match {
      case that: ConvertedFileFilter => this.f == that.f
      case _                         => false
    }
    override def hashCode: Int = f.hashCode
    override def toString: String = s"ConvertedFileFilter($f)"
  }
  private implicit class PathOps(val p: NioPath) extends AnyVal {
    def abs: NioPath = if (p.isAbsolute) p else p.toAbsolutePath
  }
  def apply(base: NioPath, range: (Int, Int), filter: NioPath => Boolean): Glob =
    new GlobImpl(base.abs, range, filter)
  private class GlobImpl(val base: NioPath,
                         val range: (Int, Int),
                         val pathFilter: NioPath => Boolean)
      extends Glob {
    private[this] val rf = new RangeFilter(base.abs, range)
    override def toString: String =
      s"Glob(\n  base = $base,\n  filter = $rf && $pathFilter,\n  depth = $range\n)"
    override def equals(o: Any): Boolean = o match {
      case that: GlobImpl =>
        this.base == that.base && this.range == that.range && this.pathFilter == that.pathFilter
      case _ => false
    }
    override def hashCode: Int = (((base.hashCode * 31) ^ filter.hashCode) * 31) ^ range.hashCode

    /**
     * The filter to apply to elements found in the file system subtree.
     *
     * @return the filter.
     */
    override def filter: NioPath => Boolean = path => rf(path) && pathFilter(path)
  }
  private[sbt] trait Builder[T] extends Any with GlobBuilder[Glob] with ToGlob {
    def repr: T
    def converter: T => NioPath
    def /(component: String): Glob = {
      val base = converter(repr).resolve(component)
      Glob(base, (0, 0), path => path.abs == base)
    }
    def \(component: String): Glob = this / component
    def glob(filter: FileFilter): Glob = Glob(converter(repr), (0, 1), filter)
    def *(filter: FileFilter): Glob = glob(filter)
    def globRecursive(filter: FileFilter): Glob = Glob(converter(repr), (0, Int.MaxValue), filter)
    def allPaths: Glob = globRecursive(AllPassFilter)
    def **(filter: FileFilter): Glob = globRecursive(filter)
    def toGlob: Glob = {
      val base = converter(repr)
      Glob(base, (0, 0), new ExactPathFilter(base))
    }
  }
  private[sbt] final class FileBuilder(val file: File) extends AnyVal with Builder[File] {
    override def repr: File = file
    override def converter: File => NioPath = (_: File).toPath
  }
  private[sbt] final class PathBuilder(val path: NioPath) extends AnyVal with Builder[NioPath] {
    override def repr: NioPath = path
    override def converter: NioPath => NioPath = identity
  }
  private[sbt] implicit class GlobOps(val glob: Glob) extends AnyVal {
    private[sbt] def toFileFilter: sbt.io.FileFilter =
      new SimpleFileFilter(file => glob.filter(file.toPath))
  }
  implicit def toPathFinder(glob: Glob): PathFinder = new io.PathFinder.GlobPathFinder(glob)
  implicit object ordering extends Ordering[Glob] {
    override def compare(left: Glob, right: Glob): Int = left.base.compareTo(right.base) match {
      // We want greater depth to come first because when we are using a Seq[Glob] to
      // register with the file system cache, it is more efficient to register the broadest glob
      // first so that we don't have to list the base directory multiple times.
      case 0 => right.range._2.compareTo(left.range._2)
      case i => i
    }
  }

  private class ExactPathFilter(val path: NioPath) extends (NioPath => Boolean) {
    override def apply(other: NioPath): Boolean = other == path
    override def equals(o: Any): Boolean = o match {
      case that: ExactPathFilter => this.path == that.path
      case _                     => false
    }
    override def hashCode: Int = path.hashCode
    override def toString: String = s"ExactPathFilter($path)"
  }
  private class RangeFilter(val base: NioPath, val range: (Int, Int)) extends (NioPath => Boolean) {
    override def apply(path: NioPath): Boolean = {
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
