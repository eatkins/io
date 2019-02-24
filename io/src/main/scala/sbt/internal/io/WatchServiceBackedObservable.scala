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
import java.nio.file.StandardWatchEventKinds.OVERFLOW
import java.nio.file.{ Path, WatchKey }
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicInteger }
import java.util.concurrent.{ CountDownLatch, TimeUnit }

import sbt.internal.io.FileEvent.{ Creation, Deletion }
import sbt.internal.io.FileTreeView.AllPass
import sbt.io._
import sbt.io.syntax._

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.Success
import scala.util.control.NonFatal

private[sbt] object WatchServiceBackedObservable {
  private val eventThreadId = new AtomicInteger(0)
}
import sbt.internal.io.WatchServiceBackedObservable._
private[sbt] class WatchServiceBackedObservable[+T](s: NewWatchState,
                                                    delay: FiniteDuration,
                                                    converter: (Path, SimpleFileAttributes) => T,
                                                    closeService: Boolean,
                                                    logger: WatchLogger)
    extends Registerable[(Path, T)]
    with Observable[(Path, T)] {
  private[this] type Event[A] = FileEvent[CustomFileAttributes[A]]
  private[this] val closed = new AtomicBoolean(false)
  private[this] val observers = new Observers[(Path, T)]
  private[this] val attributeConverter: (Path, SimpleFileAttributes) => CustomFileAttributes[T] =
    (p, a) => CustomFileAttributes.get(p, a, converter(p, a))
  private[this] val fileCache = new FileCache(attributeConverter)
  private[this] val view: NioFileTreeView[CustomFileAttributes[T]] =
    FileTreeView.DEFAULT.map(attributeConverter)
  private[this] val thread: Thread = {
    //val entryConverter = Entry.converter(converter)
    val latch = new CountDownLatch(1)
    new Thread(s"watch-state-event-thread-${eventThreadId.incrementAndGet()}") {
      setDaemon(true)
      start()
      if (!latch.await(5, TimeUnit.SECONDS))
        throw new IllegalStateException("Couldn't start event thread")
      @tailrec
      final def loopImpl(): Unit = {
        try {
          if (!closed.get) getFilesForKey(s.service.poll(delay)).foreach { event =>
            event.attributes.value.foreach(t => observers.onNext(event.path -> t))
          }
        } catch {
          case NonFatal(e) =>
            logger.debug(
              s"Error getting files from ${s.service}: $e\n${e.getStackTrace mkString "\n"}")
        }
        if (!closed.get) loopImpl()
      }
      override def run(): Unit = {
        latch.countDown()
        try {
          loopImpl()
        } catch {
          case _: InterruptedException =>
        }
      }

      def getFilesForKey(key: WatchKey): Seq[Event[T]] = key match {
        case null => Vector.empty
        case k =>
          val rawEvents = k.synchronized {
            val events = k.pollEvents.asScala.toVector
            k.reset()
            events
          }
          val keyPath = k.watchable.asInstanceOf[Path]
          val allEvents: Seq[Event[T]] = rawEvents.flatMap {
            case e if e.kind.equals(OVERFLOW) =>
              fileCache.refresh(keyPath ** AllPassFilter)
            case e if !e.kind.equals(OVERFLOW) && e.context != null =>
              val path = keyPath.resolve(e.context.asInstanceOf[Path])
              SimpleFileAttributes.get(path) match {
                case Success(attrs) => fileCache.update(path, attrs)
                case _              => Nil
              }
            case _ => Nil: Seq[Event[T]]
          }
          logger.debug(s"Received events:\n${allEvents.mkString("\n")}")
          def process(event: Event[T]): Seq[Event[T]] = {
            event match {
              case Creation(path, attrs, _) if attrs.isDirectory =>
                s.register(path)
                event +: view.list(path * AllPassFilter, AllPass).flatMap {
                  case (p, a) =>
                    process(Creation(p, a))
                }
              case Deletion(p, a, _) if a.isDirectory =>
                val events = fileCache.refresh(p ** AllPassFilter)
                events.view.filter(_.attributes.isDirectory).foreach(e => s.unregister(e.path))
                events
              case e => e :: Nil
            }
          }
          allEvents.flatMap(process)
      }

    }
  }
  override def addObserver(observer: Observer[(Path, T)]): AutoCloseable =
    observers.addObserver(observer)

  override def close(): Unit = {
    if (closed.compareAndSet(false, true)) {
      thread.interrupt()
      thread.join(5.seconds.toMillis)
      if (closeService) s.service.close()
      logger.debug("Closed WatchServiceBackedObservable")
    }
  }

  override def register(glob: Glob): Either[IOException, Observable[(Path, T)]] = {
    try {
      fileCache.register(glob)
      fileCache.list(glob.withFilter(AllPassFilter), _.isDirectory) foreach {
        case (p, _) =>
          s.register(p)
      }
      new RegisterableObservable(observers).register(glob)
    } catch {
      case e: IOException => Left(e)
    }
  }
}
