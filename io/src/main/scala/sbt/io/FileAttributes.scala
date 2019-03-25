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

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{ Files, NoSuchFileException, Path => NioPath }

import scala.util.Try

/**
 * Represents a subset of BasicFileAttributes that can sometimes be evaluated without running
 * stat on the file as part of listing a directory.
 */
private[sbt] trait FileAttributes {
  def exists: Boolean
  def isDirectory: Boolean
  def isRegularFile: Boolean
  def isSymbolicLink: Boolean
}
private[sbt] object FileAttributes {
  def get(path: NioPath): Try[FileAttributes] =
    Try {
      val attrs = Files.readAttributes(path, classOf[BasicFileAttributes])
      new Impl(true, attrs.isDirectory, attrs.isRegularFile, attrs.isSymbolicLink)
    }.recover {
      case _: NoSuchFileException => new Impl(false, false, false, false)
    }
  private[sbt] def get(exists: Boolean,
                       isDirectory: Boolean,
                       isRegularFile: Boolean,
                       isSymbolicLink: Boolean): FileAttributes =
    new Impl(exists, isDirectory, isRegularFile, isSymbolicLink)
  private class Impl(override val exists: Boolean,
                     override val isDirectory: Boolean,
                     override val isRegularFile: Boolean,
                     override val isSymbolicLink: Boolean)
      extends FileAttributes {
    override def equals(o: Any): Boolean = o match {
      case that: FileAttributes =>
        (this.isDirectory == that.isDirectory) && (this.isRegularFile == that.isRegularFile) && (this.isSymbolicLink == that.isSymbolicLink)
      case _ => false
    }
    override def hashCode: Int =
      ((isDirectory.hashCode * 31) ^ (isRegularFile.hashCode * 31)) ^ (isSymbolicLink.hashCode * 31)
    override def toString: String =
      s"FileAttributes(isDirectory = $isDirectory, isRegularFile = $isRegularFile" +
        s", isSymbolicLink = $isSymbolicLink, exists = $exists)"
  }
}
