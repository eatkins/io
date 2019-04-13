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

import sbt.io.WatchService
import sbt.nio.{ FileAttributes, FileTreeView }

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
    extends FileTreeView.Nio[T]
    with Registerable[FileEvent[T]]
    with Observable[FileEvent[T]]
    with AutoCloseable

private[sbt] object FileTreeRepository {

  /**
   * Create a [[FileTreeRepository]]. The generated repository will cache the file system tree for the
   * monitored directories.
   *
   * @return a file repository.
   */
  private[sbt] def default: FileTreeRepository[FileAttributes] = new FileTreeRepositoryImpl

  /**
   * Create a [[FileTreeRepository]]. The generated repository will cache the file system tree for the
   * monitored directories.
   *
   * @return a file repository.
   */
  private[sbt] def legacy: FileTreeRepository[FileAttributes] =
    new LegacyFileTreeRepository((_: Any) => (), WatchService.default)

  /**
   * Create a [[FileTreeRepository]] with a provided logger. The generated repository will cache
   * the file system tree for the monitored directories.
   *
   * @param logger used to log file events
   * @param watchService the [[WatchService]] to monitor for file system events
   * @tparam T the generic type of the custom file attributes
   * @return a file repository.
   */
  private[sbt] def legacy[T](logger: WatchLogger,
                             watchService: WatchService): FileTreeRepository[FileAttributes] =
    new LegacyFileTreeRepository(logger, watchService)

}
