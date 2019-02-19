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
import java.nio.file.Path

import sbt.internal.io.FileEvent.{ Deletion, Update }
import sbt.internal.io.HybridPollingFileTreeRepository._
import sbt.io._
import sbt.io.syntax._

import scala.concurrent.duration.FiniteDuration
import scala.util.Success

/**
 * A hybrid [[FileTreeRepository]] that caches some paths and monitors them with os notifications and
 * does not cache the paths that are filtered using the provided shouldPoll function. As a general
 * rule, the paths to be polled should ideally not be in the same directory tree as any of the
 * paths that are being cached. The [[FileTreeRepository.list]] method should do the right thing in this
 * case, but it's possible that there may be some bugs in handling the overlapping paths.
 *
 * @tparam T the type of the data values associated with each paths.
 */
private[sbt] trait HybridPollingFileTreeRepository[+T]
    extends FileTreeRepository[CustomFileAttributes[T]] { self =>
  def toPollingObservable(delay: FiniteDuration,
                          logger: WatchLogger): Observable[Event[T]] with Registerable[Event[T]]
}

private[io] case class HybridPollingFileTreeRepositoryImpl[+T](
    converter: (Path, SimpleFileAttributes) => CustomFileAttributes[T],
    pollingGlobs: Seq[Glob])
    extends HybridPollingFileTreeRepository[T] { self =>
  private val repo = new FileTreeRepositoryImpl[T](converter)
  private val view: NioFileTreeView[CustomFileAttributes[T]] =
    DefaultFileTreeView.map((p: Path, a: SimpleFileAttributes) => converter(p, a))
  private[this] val observers = new Observers[(Path, Event[T])]
  private[this] val handle = repo.addObserver(observers)

  private def shouldPoll(glob: Glob): Boolean =
    pollingGlobs.exists(_.toFileFilter(acceptBase = true).accept(glob.base.toFile))
  override def addObserver(
      observer: Observer[(Path, FileEvent[CustomFileAttributes[T]])]): AutoCloseable = {
    observers.addObserver(observer)
  }
  override def list(glob: Glob, filter: ((Path, CustomFileAttributes[T])) => Boolean)
    : Seq[(Path, CustomFileAttributes[T])] = {
    if (!shouldPoll(glob)) {
      /*
       * The repository may contain some paths that require polling to access. We must remove
       * those entries from the result. For every one of these entries that is a directory, we
       * must poll that directory and add its result to the list. If the entry is a regular file,
       * then we need to poll just that file.
       */
      val (needPoll, ready) = repo.list(glob, filter).partition {
        case (path: Path, _) =>
          shouldPoll(Glob(path.toFile, -1, new ExactFileFilter(path.toFile)))
      }
      ready ++ needPoll
        .filter(_._2.isDirectory)
        .sortBy(_._1)
        .headOption
        .toSeq
        .flatMap {
          case pair @ (path: Path, _: CustomFileAttributes[T]) =>
            val depth =
              if (glob.depth == Integer.MAX_VALUE) Integer.MAX_VALUE
              else glob.depth - path.relativize(path).getNameCount - 1
            Seq(pair) ++ view.list((pair._1 ** AllPassFilter).withDepth(depth), filter)
        }
    } else {
      view.list(glob, filter)
    }
  }

  override def close(): Unit = {
    handle.close()
    repo.close()
  }
  override def toPollingObservable(
      delay: FiniteDuration,
      logger: WatchLogger): Observable[Event[T]] with Registerable[Event[T]] =
    new Observable[Event[T]] with Registerable[Event[T]] {
      private val childObservers = new Observers[Event[T]]
      private val expandedChildObservers = new Observer[(Path, Event[T])] {
        override def onNext(t: (Path, Event[T])): Unit = {
          childObservers.onNext(t._2)
        }
      }
      private val watchState = WatchState.empty(pollingGlobs, new PollingWatchService(delay))
      private val observable: Observable[(Path, Event[T])] with Registerable[(Path, Event[T])] =
        new WatchServiceBackedObservable[Event[T]](
          watchState,
          delay,
          (path: Path, simpleFileAttributes: SimpleFileAttributes) =>
            Success {
              val attributes = converter(path, simpleFileAttributes)
              if (attributes.exists) Update(path, attributes, attributes)
              else Deletion(path, attributes)
          },
          true,
          logger
        )
      val handle: AutoCloseable = {
        val handles = Seq(
          observers.addObserver(expandedChildObservers),
          observable.addObserver(observers)
        )
        new AutoCloseable { override def close(): Unit = handles.foreach(_.close()) }
      }

      pollingGlobs.foreach(observable.register(_).right.foreach(_.close()))

      override def register(glob: Glob): Either[IOException, Observable[Event[T]]] = {
        if (shouldPoll(glob))
          observable.register(glob).right.map(o => Observable.map(o, (_: (Path, Event[T]))._2))
        else Registerable.fileEvent(glob, childObservers)
      }
      override def close(): Unit = {
        handle.close()
        observable.close()
      }
      override def addObserver(observer: Observer[Event[T]]): AutoCloseable = {
        childObservers.addObserver(observer)
      }
    }

  /**
   * Register a glob for monitoring.
   *
   * @param glob Glob
   * @return an Either that is a Right when register has no errors and a Left if an IOException is
   *         thrown while registering the path. The result should be true if the path has
   *         never been previously registered or if the recursive flag flips from false to true.
   */
  override def register(glob: Glob): Either[IOException, Observable[(Path, Event[T])]] = {
    repo.register(glob)
    new RegisterableObservable(observers).register(glob)
  }
}

private[sbt] object HybridPollingFileTreeRepository {
  type Event[+T] = FileEvent[CustomFileAttributes[T]]
  def apply[T](converter: (Path, SimpleFileAttributes) => CustomFileAttributes[T],
               pollingGlobs: Glob*): HybridPollingFileTreeRepository[T] =
    HybridPollingFileTreeRepositoryImpl(converter, pollingGlobs)
}
