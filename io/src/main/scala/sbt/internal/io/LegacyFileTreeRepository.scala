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

import java.io.IOException
import java.nio.file.{ Path, WatchKey }
import java.util.concurrent.ConcurrentHashMap

import sbt.io._

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.Try

/**
 * A [[FileTreeRepository]] that can be used to emulate the behavior of sbt < 1.3.0 The list methods
 * will always poll the file system and the monitoring will be handled by a
 * [[WatchServiceBackedObservable]].
 */
private[sbt] class LegacyFileTreeRepository[T](converter: (Path, SimpleFileAttributes) => Try[T],
                                               logger: WatchLogger,
                                               watchService: WatchService)
    extends FileTreeRepository[(SimpleFileAttributes, Try[T])] {
  private[this] val view: NioFileTreeView[(SimpleFileAttributes, Try[T])] =
    FileTreeView.DEFAULT.map((p: Path, a: SimpleFileAttributes) => a -> converter(p, a))
  private[this] val globs = ConcurrentHashMap.newKeySet[Glob].asScala
  private[this] val fileCache = new FileCache(converter, globs)
  private[this] val observable: Observable[FileEvent[(SimpleFileAttributes, Try[T])]]
    with Registerable[FileEvent[(SimpleFileAttributes, Try[T])]] =
    new WatchServiceBackedObservable[T](
      new NewWatchState(globs, watchService, new ConcurrentHashMap[Path, WatchKey].asScala),
      100.millis,
      converter,
      closeService = true,
      logger
    )
  private[this] val observers = new Observers[FileEvent[(SimpleFileAttributes, Try[T])]]
  private[this] val handle =
    observable.addObserver(new Observer[FileEvent[(SimpleFileAttributes, Try[T])]] {
      override def onNext(event: FileEvent[(SimpleFileAttributes, Try[T])]): Unit = {
        val events: Seq[FileEvent[(SimpleFileAttributes, Try[T])]] =
          fileCache.update(event.path, event.attributes._1)
        events.foreach(observers.onNext)
      }
    })
  override def close(): Unit = {
    handle.close()
    observable.close()
  }
  override def register(
      glob: Glob): Either[IOException, Observable[FileEvent[(SimpleFileAttributes, Try[T])]]] = {
    fileCache.register(glob)
    observable.register(glob).right.foreach(_.close())
    new RegisterableObservable(observers).register(glob)
  }
  override def list(glob: Glob, filter: ((Path, (SimpleFileAttributes, Try[T]))) => Boolean)
    : Seq[(Path, (SimpleFileAttributes, Try[T]))] =
    view.list(glob, filter)

  /**
   * Add callbacks to be invoked on file events.
   *
   * @param observer the callbacks to invoke
   * @return a handle to the callback.
   */
  override def addObserver(
      observer: Observer[FileEvent[(SimpleFileAttributes, Try[T])]]): AutoCloseable =
    observers.addObserver(observer)
}
