package sbt
package internal
package io

import java.nio.file.{ Files, Path, WatchKey }
import java.util.concurrent.{ ConcurrentHashMap, CountDownLatch, TimeUnit }

import org.scalatest.FlatSpec
import sbt.io._
import sbt.io.syntax._

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.Success

class WatchServiceBackedObservableSpec extends FlatSpec {
  "register" should "work recursively" in IO.withTemporaryDirectory { dir =>
    val path = dir.toPath
    val subdir = Files.createDirectories(path.resolve("a").resolve("b").resolve("c")).toRealPath()
    val watchState =
      new NewWatchState(ConcurrentHashMap.newKeySet[Glob].asScala,
                        WatchService.default,
                        new ConcurrentHashMap[Path, WatchKey].asScala)
    val observable =
      new WatchServiceBackedObservable(
        watchState,
        100.millis,
        (_: Path, _: FileAttributes) => Success(()),
        closeService = true,
        (_: Any) => {}
      )
    try {
      val latch = new CountDownLatch(1)
      val file = subdir.resolve("file")
      observable.addObserver(e => if (e.path == file) latch.countDown())
      observable.register(path ** AllPassFilter)
      Files.createFile(file)
      assert(latch.await(1, TimeUnit.SECONDS))
    } finally observable.close()
  }
}
