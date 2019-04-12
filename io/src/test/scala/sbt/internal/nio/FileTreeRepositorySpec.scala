package sbt.internal.nio

import java.nio.file.attribute.FileTime
import java.nio.file.{ Files, Path => NioPath, Paths => NioPaths }
import java.util.concurrent.{ ConcurrentHashMap, CountDownLatch, TimeUnit }

import org.scalatest.{ FlatSpec, Matchers }
import sbt.internal.nio.FileEvent.{ Creation, Deletion }
import sbt.io.IO
import sbt.nio.syntax._
import sbt.nio.{ FileAttributes, Glob }

import scala.collection.JavaConverters._
import scala.concurrent.duration._

object FileTreeRepositorySpec {
  implicit class FileRepositoryOps[T](val fileCache: FileTreeRepository[FileAttributes])
      extends AnyVal {
    def ls(glob: Glob): Seq[NioPath] = fileCache.list(glob).map(_._1)
  }
  implicit class CountdownLatchOps(val latch: CountDownLatch) extends AnyVal {
    def await(duration: Duration): Boolean = latch.await(duration.toNanos, TimeUnit.NANOSECONDS)
  }
  private val DEFAULT_TIMEOUT = 1.second
  def using[T, R](fileCache: => FileTreeRepository[T])(f: FileTreeRepository[T] => R): R = {
    val cache = fileCache
    try f(cache)
    finally cache.close()
  }
  def withTempDir[R](f: NioPath => R): R =
    IO.withTemporaryDirectory(dir => f(dir.toPath.toRealPath()))
  def withTempDir[R](dir: NioPath)(f: NioPath => R): R = {
    val subdir = Files.createTempDirectory(dir, "tmp")
    try f(subdir)
    finally IO.delete(subdir.toFile)
  }
  def withTempFile[R](dir: NioPath)(f: NioPath => R): R = {
    val file = Files.createTempFile(dir, "", "").toRealPath()
    try {
      f(file)
    } finally {
      Files.deleteIfExists(file)
      ()
    }
  }
  def withTempFile[R](f: NioPath => R): R = withTempDir(withTempFile(_)(f))
  def simpleCache(f: NioPath => Unit): FileTreeRepository[FileAttributes] =
    simpleCache(new Observer[FileEvent[FileAttributes]] {
      override def onNext(t: FileEvent[FileAttributes]): Unit = f(t.path)
    })
  def simpleCache(
      observer: Observer[FileEvent[FileAttributes]]): FileTreeRepository[FileAttributes] = {
    val underlying = new FileTreeRepositoryImpl()
    underlying.addObserver(observer)
    underlying
  }
  case class LastModified(at: Long)
}
class FileTreeRepositorySpec extends FlatSpec with Matchers {
  import FileTreeRepositorySpec._
  "register" should "see existing files" in withTempFile { file =>
    using(simpleCache((_: NioPath) => {})) { c =>
      val glob = file.getParent / *
      c.register(glob)
      c.ls(glob) shouldBe Seq(file)
    }
  }
  it should "detect new files" in withTempDir { dir =>
    val latch = new CountDownLatch(1)
    val file = dir.resolve("file")
    using(simpleCache((p: NioPath) => if (p == file) latch.countDown())) { c =>
      c.register(dir / **)
      Files.createFile(file)
      assert(latch.await(DEFAULT_TIMEOUT))
      c.ls(dir / **) shouldBe Seq(file)
    }
  }
  it should "detect new subdirectories" in withTempDir { dir =>
    val latch = new CountDownLatch(1)
    val subdir = dir.resolve("subdir")
    using(simpleCache((p: NioPath) => if (p == subdir) latch.countDown())) { c =>
      c.register(dir / **)
      Files.createDirectories(subdir)
      assert(latch.await(DEFAULT_TIMEOUT))
      c.ls(dir / *) shouldBe Seq(subdir)
    }
  }
  it should "detect move events" in withTempDir { dir =>
    val latch = new CountDownLatch(1)
    val initial = Files.createTempFile(dir, "move", "")
    val moved = NioPaths.get(s"${initial.toString}.moved")
    val observer: Observer[FileEvent[FileAttributes]] =
      (_: FileEvent[FileAttributes]) match {
        case Creation(path, _) => if (path == moved) latch.countDown()
        case _                 =>
      }
    using(simpleCache(observer)) { c =>
      c.register(dir / **)
      c.ls(dir / *) === Seq(initial)
      Files.move(initial, moved)
      assert(latch.await(DEFAULT_TIMEOUT))
      c.ls(dir / *) === Seq(moved)
    }
  }
  it should "ignore children of subdirectories when recursive flag is false" in withTempDir { dir =>
    withTempDir(dir) { subdir =>
      val fileLatch = new CountDownLatch(1)
      val subdirLatch = new CountDownLatch(1)
      using(simpleCache((path: NioPath) => {
        if (path.startsWith(subdir) && path != subdir) fileLatch.countDown()
        else if (path == subdir && Files.getLastModifiedTime(path).toMillis == 2000)
          subdirLatch.countDown()
      })) { c =>
        c.register(dir / *)
        withTempFile(subdir) { f =>
          assert(Files.exists(f))
          assert(fileLatch.getCount == 1) // The child creation should not have triggered a callback
          Files.setLastModifiedTime(subdir, FileTime.fromMillis(2000))
          assert(subdirLatch.await(DEFAULT_TIMEOUT))
          c.ls(dir / **) === Seq(subdir)
        }
      }
    }
  }
  it should "add recursive flag when previously set to false" in withTempDir { dir =>
    withTempDir(dir) { subdir =>
      withTempFile(subdir) { f =>
        using(simpleCache((_: NioPath) => {})) { c =>
          c.register(dir / *)
          c.ls(dir / **).toSet shouldBe Set(subdir)
          c.register(dir / **)
          c.ls(dir / **).toSet shouldBe Set(subdir, f)
        }
      }
    }
  }
  it should "not remove recursive flag when already set" in withTempDir { dir =>
    withTempDir(dir) { subdir =>
      withTempFile(subdir) { f =>
        using(simpleCache((_: NioPath) => {})) { c =>
          c.register(dir / **)
          c.ls(dir / **).toSet shouldBe Set(subdir, f)
          c.register(dir / *)
          c.ls(dir / **).toSet shouldBe Set(subdir, f)
        }
      }
    }
  }

  it should "detect many creations and deletions" in withTempDir { dir =>
    val base = dir.toRealPath()
    val dirsToAdd = 400
    val filesToAdd = 5
    val subdirs = (0 until dirsToAdd).map(i => base.resolve(s"subdir-$i"))
    val files = subdirs.flatMap { subdir =>
      (0 to filesToAdd).map(i => subdir.resolve(s"file-$i"))
    }
    val creationLatch = new CountDownLatch(subdirs.length + files.length)
    val deletionLatch = new CountDownLatch(subdirs.length + files.length)
    val creationLatches = new ConcurrentHashMap[NioPath, CountDownLatch].asScala
    val deletionLatches = new ConcurrentHashMap[NioPath, CountDownLatch].asScala
    (subdirs ++ files).foreach { f =>
      creationLatches.put(f, creationLatch)
      deletionLatches.put(f, deletionLatch)
    }
    val observer: Observer[FileEvent[FileAttributes]] =
      (_: FileEvent[FileAttributes]) match {
        case Creation(p, _) => creationLatches.remove(p).foreach(_.countDown())
        case Deletion(p, _) => deletionLatches.remove(p).foreach(_.countDown())
        case _              =>
      }
    using(simpleCache(observer)) { c =>
      c.register(dir / **)

      withThread("file-creation-thread") {
        subdirs.foreach { dir =>
          Files.createDirectories(dir)
        }
        files.foreach { f =>
          Files.createFile(f)
        }
      } {
        assert(creationLatch.await(DEFAULT_TIMEOUT * 10))
        c.ls(dir / **).toSet shouldBe (files ++ subdirs).toSet
      }

      withThread("file-deletion-thread") {
        subdirs.foreach(p => IO.delete(p.toFile))
      } {
        if (!deletionLatch.await(DEFAULT_TIMEOUT * 10)) {
          assert(deletionLatch.getCount == 0)
        }
        c.ls(dir / *) shouldBe 'empty
      }
    }
  }

  "updates" should "be detected" in withTempFile { file =>
    val latch = new CountDownLatch(1)
    val updatedLastModified = LastModified(2000L)
    using(FileTreeRepository.default.map((p: NioPath, _: FileAttributes) => {
      LastModified(Files.getLastModifiedTime(p).toMillis)
    }, closeUnderlying = true)) { c =>
      c.addObserver { event =>
        val lastModified = event.attributes
        if (event.exists && lastModified == updatedLastModified) latch.countDown()
      }
      c.register(file.getParent / **)
      val Seq(fileEntry) = c.list(file.getParent / **)
      val lastModified = fileEntry._2
      lastModified shouldBe LastModified(Files.getLastModifiedTime(file).toMillis)
      Files.setLastModifiedTime(file, FileTime.fromMillis(updatedLastModified.at))
      assert(latch.await(DEFAULT_TIMEOUT))
      val Seq(newFileEntry) = c.list(file.getParent / **)
      newFileEntry._2 shouldBe updatedLastModified
    }
  }
  private def withThread[R](name: String)(body: => Unit)(f: => R): Unit = {
    val thread = new Thread(s"FileTreeRepositorySpec-$name") {
      override def run(): Unit = body
      setDaemon(true)
      start()
    }
    try {
      f
      ()
    } finally {
      thread.interrupt()
      thread.join(5000L)
    }
  }
}
