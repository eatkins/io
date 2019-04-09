package sbt.nio

import java.nio.file.Files

import org.scalatest.FlatSpec
import sbt.io.IO
import sbt.nio.syntax._

class TraversableGlobSpec extends FlatSpec {
  "Traversable globs" should "collect multiple directories" in {
    IO.withTemporaryDirectory { dirFile =>
      val dir = dirFile.toPath
      val subdir = Files.createDirectories(dir.resolve("subdir"))
      val otherSubdir = Files.createDirectories(dir.resolve("other-subdir"))
      val nestedSubdir = Files.createDirectory(otherSubdir.resolve("nested"))
      val subdirs = Seq(subdir, otherSubdir, nestedSubdir)
      val files = subdirs.map(d => Files.createFile(d.resolve("file")))
      val globs = subdirs.dropRight(1).map(_ / **)

      val actual = Glob.all(globs, FileTreeView.DEFAULT_NIO, (_, _) => true).map(_._1).toSet
      val expected = (files :+ nestedSubdir).toSet
      assert(actual == expected)
    }
  }
  it should "handle redundant globs" in {
    IO.withTemporaryDirectory { dirFile =>
      val dir = dirFile.toPath
      val subdir = Files.createDirectories(dir.resolve("subdir"))
      val file = Files.createFile(subdir.resolve("file.txt"))
      val globs = Seq[Glob](dir / **, dir / ** / *.txt, subdir / * / *.txt, Glob(file))
      val actual = Glob.all(globs, FileTreeView.DEFAULT_NIO, (_, _) => true).map(_._1).sorted
      val expected = Seq(subdir, file)
      assert(actual == expected)
    }
  }
  it should "handle semi-overlapping globs" in {
    IO.withTemporaryDirectory { dirFile =>
      val dir = dirFile.toPath
      val subdir = Files.createDirectories(dir.resolve("subdir"))
      val nested = Files.createDirectories(subdir.resolve("nested"))
      val deeply = Files.createDirectories(nested.resolve("deeply"))
      val txtFile = Files.createFile(nested.resolve("file.txt"))
      val mdFile = Files.createFile(deeply.resolve("file.md"))
      val globs = Seq[Glob](dir / ** / *.md, subdir / *.txt, nested / *.md, nested / *.txt)
      val actual = Glob.all(globs, FileTreeView.DEFAULT_NIO, (_, _) => true).map(_._1).sorted
      val expected = Seq(mdFile, txtFile)
      assert(actual == expected)
    }
  }
}
