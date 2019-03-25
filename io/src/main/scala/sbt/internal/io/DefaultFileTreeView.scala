/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.internal.io

import java.nio.file.{ NoSuchFileException, NotDirectoryException, Path => NioPath }

import com.swoval.files.{ FileTreeViews, TypedPath }
import com.swoval.functional.Filter
import sbt.io._

import scala.collection.JavaConverters._
import SwovalConverters._

private[sbt] object DefaultFileTreeView extends NioFileTreeView[FileAttributes] {
  private[this] val fileTreeView =
    if ("nio" == sys.props.getOrElse("sbt.pathfinder", ""))
      FileTreeViews.getNio(true)
    else
      FileTreeViews.getDefault(true)

  override def list(
      glob: Glob,
      filter: ((NioPath, FileAttributes)) => Boolean): Seq[(NioPath, FileAttributes)] =
    Retry {
      try {
        fileTreeView
          .list(glob.base, glob.range.toSwovalDepth, new Filter[TypedPath] {
            override def accept(t: TypedPath): Boolean = true
          })
          .asScala
          .flatMap { typedPath =>
            val path = typedPath.getPath
            val attributes = FileAttributes.get(typedPath.exists,
                                                typedPath.isDirectory,
                                                typedPath.isFile,
                                                typedPath.isSymbolicLink)
            val pair = path -> attributes
            if (glob.filter(path) && filter(pair)) Some(pair) else None
          }
          .toIndexedSeq
      } catch {
        case _: NoSuchFileException | _: NotDirectoryException =>
          Nil
      }
    }
}
