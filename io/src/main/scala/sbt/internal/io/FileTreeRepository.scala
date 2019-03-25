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

import java.nio.file.{ Path => NioPath }

import sbt.io.{ FileAttributes, FileTreeView, Glob, WatchService }

import scala.util.Try

private[sbt] trait NioFileTreeView[+T] extends FileTreeView[(NioPath, T)] {
  def list(glob: Glob, filter: ((NioPath, T)) => Boolean): Seq[(NioPath, T)]
  final def list(glob: Glob, filter: (NioPath, T) => Boolean): Seq[(NioPath, T)] =
    list(glob, filter.tupled)
}

// scaladoc is horrible and I couldn't figure out how to link the overloaded method listEntries
// in this message.
/**
 * Monitors registered directories for file changes. A typical implementation will keep an
 * in memory cache of the file system that can be queried in [[FileTreeRepository!.list]]. The
 * [[FileTreeRepository#register]] method adds monitoring for a particular cache. A filter may be
 * provided so that the cache doesn't waste memory on files the user doesn't care about. The
 * cache may be shared across a code base so there additional apis for adding filters or changing
 * the recursive property of a directory.
 *
 * @tparam T the type of the
 */
private[sbt] trait FileTreeRepository[+T]
    extends NioFileTreeView[T]
    with Registerable[FileEvent[T]]
    with Observable[FileEvent[T]]
    with AutoCloseable

private[sbt] object FileTreeRepository {

  /**
   * Create a [[FileTreeRepository]]. The generated repository will cache the file system tree for the
   * monitored directories.
   *
   * @param converter function to generate an instance of `T` from a
   *                  `(Path, BasicFileAttributes)` pair
   * @tparam T the generic type of the data value associated with each file
   * @return a file repository.
   */
  private[sbt] def default[T: Manifest](converter: (NioPath, FileAttributes) => Try[T])
    : FileTreeRepository[(FileAttributes, Try[T])] =
    new FileTreeRepositoryImpl[T](converter)

  /**
   * Create a [[FileTreeRepository]]. The generated repository will cache the file system tree for the
   * monitored directories.
   *
   * @param converter function to generate a cache data value from a
   *                  `(Path, FileAttributes)` pair
   * @tparam T the generic type of the
   * @return a file repository.
   */
  private[sbt] def legacy[T](converter: (NioPath, FileAttributes) => Try[T])
    : FileTreeRepository[(FileAttributes, Try[T])] =
    new LegacyFileTreeRepository[T](converter, new WatchLogger {
      override def debug(msg: => Any): Unit = {}
    }, WatchService.default)

  /**
   * Create a [[FileTreeRepository]] with a provided logger. The generated repository will cache
   * the file system tree for the monitored directories.
   *
   * @param converter function to generate a cache data value from a
   *                  `(Path, FileAttributes)` pair
   * @param logger used to log file events
   * @param watchService the [[WatchService]] to monitor for file system events
   * @tparam T the generic type of the custom file attributes
   * @return a file repository.
   */
  private[sbt] def legacy[T](
      converter: (NioPath, FileAttributes) => Try[T],
      logger: WatchLogger,
      watchService: WatchService): FileTreeRepository[(FileAttributes, Try[T])] =
    new LegacyFileTreeRepository[T](converter, logger, watchService)

  /**
   * Create a [[FileTreeRepository]]. The generated repository will cache the file system tree for some
   * of the paths under monitoring, but others will need to be polled.
   *
   * @param converter function to generate a cache data value from a
   *                  `(Path, FileAttributes)` pair
   * @param pollingGlobs do not cache any path contained in these [[Glob]]s.
   * @tparam T the generic type of the custom file attributes
   * @return a file repository.
   */
  private[sbt] def hybrid[T](converter: (NioPath, FileAttributes) => Try[T],
                             pollingGlobs: Glob*): HybridPollingFileTreeRepository[T] =
    HybridPollingFileTreeRepository(converter, pollingGlobs: _*)
}
