/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.internal.nio

import java.nio.file._
import java.util

import com.swoval.files.{ FileTreeViews, TypedPath }
import sbt.internal.io.Retry
import sbt.nio.{ FileAttributes, FileTreeView, Glob }

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.VectorBuilder

private[sbt] object DefaultFileTreeView extends FileTreeView.Nio[FileAttributes] {
  private[this] val fileTreeView =
    if ("nio" == sys.props.getOrElse("sbt.pathfinder", ""))
      FileTreeViews.getNio(true)
    else
      FileTreeViews.getDefault(true)

  private def listImpl(glob: Glob): Vector[(Path, FileAttributes)] = {
    val result = new VectorBuilder[(Path, FileAttributes)]
    val queue = new util.LinkedList[Path]
    queue.add(glob.base)
    def toFileAttributes(typedPath: TypedPath): FileAttributes =
      FileAttributes(isDirectory = typedPath.isDirectory,
                     isOther = false,
                     isRegularFile = typedPath.isFile,
                     isSymbolicLink = typedPath.isSymbolicLink)
    def maybeAdd(typedPath: TypedPath): Unit = {
      val path = typedPath.getPath
      if (glob.filter(path)) result += path -> toFileAttributes(typedPath)
    }
    @tailrec
    def impl(): Unit = {
      queue.poll match {
        case null =>
        case path =>
          val depth = if (glob.base == path) 0 else glob.base.relativize(path).getNameCount
          fileTreeView.list(path, 0, (_: TypedPath) => true).asScala.foreach {
            case p if p.isDirectory =>
              if (depth + 1 <= glob.range._2) queue.add(p.getPath)
              maybeAdd(p)
            case f => maybeAdd(f)
          }
          impl()
      }
    }
    impl()
    result.result
  }
  override def list(glob: Glob): Seq[(Path, FileAttributes)] =
    Retry {
      try {
        val base = glob.base
        (if (glob.range._1 == 0) {
           if (glob.filter(base)) FileAttributes(base) match {
             case Right(a) => Vector(base -> a)
             case _        => Vector.empty
           } else Vector.empty
         } else Vector.empty[(Path, FileAttributes)]) ++ {
          if (glob.range._2 > 0)
            listImpl(glob)
          else
            Vector
              .empty[(Path, FileAttributes)]
        }
      } catch {
        case _: NoSuchFileException | _: NotDirectoryException =>
          Nil
      }
    }
}
