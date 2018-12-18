package sbt.io

import java.nio.file.Files

import org.scalatest.FlatSpec
import syntax._

class PathFinderInputSpec extends FlatSpec {
  "PathFinderInput" should "provide the same results as Pathfinder" in IO.withTemporaryDirectory {
    dir =>
      assert(dir.get() == Seq(dir))
  }
  it should "work with globbing" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "foo.txt")
    file.createNewFile()
    assert(file.get() == Seq(file))
    assert((dir glob AllPassFilter).get() == (PathFinder(dir) glob AllPassFilter).get())
    assert((dir glob NothingFilter).get() == Nil)
    assert((dir glob "foo.txt").get() == Seq(file))
    assert((dir glob "bar.txt").get() == Nil)
  }
  it should "work with recursive globbing" in IO.withTemporaryDirectory { dir =>
    val subdir = Files.createDirectories(dir.toPath.resolve("subdir")).toFile
    val file = new File(subdir, "foo.txt")
    file.createNewFile()
    assert(file.get() == Seq(file))
    assert((dir ** AllPassFilter).get().toSet == (PathFinder(dir) ** AllPassFilter).get().toSet)
    assert((dir ** NothingFilter).get() == Nil)
    assert((dir ** "foo.txt").get() == Seq(file))
    assert((dir ** "bar.txt").get() == Nil)
    assert(
      dir.descendantsExcept(AllPassFilter, NothingFilter).get().toSet ==
        PathFinder(dir).descendantsExcept(AllPassFilter, NothingFilter).get().toSet)
  }
  it should "work with combiners" in IO.withTemporaryDirectory { dir =>
    val subdir = Files.createDirectories(dir.toPath.resolve("subdir")).toFile
    val file = new File(subdir, "foo.txt")
    file.createNewFile()
    val combined = dir +++ file
    assert(combined.get() == Seq(dir, file))
    val excluded = dir.allPaths --- file
    assert(excluded.get() == Seq(dir, subdir))
  }
}
