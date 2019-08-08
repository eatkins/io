/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.nio

import java.nio.file.Files

import org.scalatest.FlatSpec
import sbt.io.IO
import sbt.nio.PathFilterSpec.PathOps
import sbt.nio.file.syntax._
import sbt.nio.file.{ FileTreeView, _ }

class IoFileFilterExtensionSpec extends FlatSpec {
  "DirectoryFilter" should "negate" in IO.withTemporaryDirectory { dir =>
    val notDirectoryFilter = !sbt.nio.file.IsDirectory
    assert(!sbt.io.DirectoryFilter == notDirectoryFilter)
    val dirPath = dir.toPath
    val file = Files.createFile(dirPath.resolve("file"))
    assert(!(!sbt.io.DirectoryFilter).accept(dirPath, FileAttributes(dirPath).right.get))
    assert((!sbt.io.DirectoryFilter).accept(file, FileAttributes(file).right.get))
  }
  it should "combine with &&" in IO.withTemporaryDirectory { dir =>
    val dirPath = dir.toPath
    val subdir = Files.createDirectories(dirPath.resolve("subdir"))
    val nested = Files.createDirectories(dirPath.resolve("nested"))
    val file = Files.createFile(subdir.resolve("file"))
    val filter = sbt.io.DirectoryFilter && (** / "nested")
    assert(FileTreeView.default.list(dirPath.toGlob / **, filter).map(_._1) == Seq(nested))
    assert(FileTreeView.default.list(dirPath.toGlob / **, !filter).map(_._1) == Seq(subdir, file))
  }
  it should "combine with ||" in IO.withTemporaryDirectory { dir =>
    val dirPath = dir.toPath
    val subdir = Files.createDirectories(dirPath.resolve("subdir"))
    val nested = Files.createDirectories(dirPath.resolve("nested"))
    val file = Files.createFile(subdir.resolve("file"))
    val otherFile = Files.createFile(subdir.resolve("other"))
    val filter = sbt.io.DirectoryFilter || (** / "file")
    assert(
      FileTreeView.default.list(dirPath.toGlob / **, filter).map(_._1).toSet ==
        Set(subdir, nested, file)
    )
    assert(FileTreeView.default.list(dirPath.toGlob / **, !filter).map(_._1) == Seq(otherFile))
  }
  "HiddenFileFilter" should "negate" in IO.withTemporaryDirectory { dir =>
    val notHiddenFileFilter = !sbt.io.HiddenFileFilter
    assert(notHiddenFileFilter == !IsHidden)
    val dirPath = dir.toPath
    val file = Files.createFile(dirPath / ".file").setHidden()
    assert(notHiddenFileFilter.accept(dirPath, FileAttributes(dirPath).right.get))
    assert(!notHiddenFileFilter.accept(file, FileAttributes(file).right.get))
  }
  it should "combine with &&" in IO.withTemporaryDirectory { dir =>
    val dirPath = dir.toPath
    val file = Files.createFile(dirPath / ".file").setHidden()
    val other = Files.createFile(dirPath / ".other").setHidden()
    val foo = Files.createFile(dirPath / "foo")
    val filter = sbt.io.HiddenFileFilter && (** / ".file")
    assert(FileTreeView.default.list(dirPath.toGlob / **, filter).map(_._1) == Seq(file))
    assert(
      FileTreeView.default.list(dirPath.toGlob / **, !filter).map(_._1).toSet == Set(other, foo)
    )
  }
  it should "combine with ||" in IO.withTemporaryDirectory { dir =>
    val dirPath = dir.toPath
    val file = Files.createFile(dirPath / ".file").setHidden()
    val other = Files.createFile(dirPath / "other")
    val foo = Files.createFile(dirPath / "foo")
    val filter = sbt.io.HiddenFileFilter || (** / "foo")
    assert(FileTreeView.default.list(dirPath.toGlob / **, filter).map(_._1).toSet == Set(file, foo))
    assert(FileTreeView.default.list(dirPath.toGlob / **, !filter).map(_._1) == Seq(other))
  }
}
