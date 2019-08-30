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

import java.io.File

import org.scalatest.FlatSpec
import sbt.io.IO
import sbt.nio.file.syntax._
import sbt.nio.file.{ Glob, RecursiveGlob }

import scala.collection.JavaConverters._

class GlobOrderingSpec extends FlatSpec {
  "Globs" should "be ordered" in IO.withTemporaryDirectory { dir =>
    val subdir = new File(dir, "subdir")
    assert(Seq(Glob(subdir), Glob(dir)).sorted == Seq(Glob(dir), Glob(subdir)))
  }
  they should "fall back on depth" in IO.withTemporaryDirectory { dir =>
    val recursive = Glob(dir, RecursiveGlob)
    val nonRecursive = Glob(dir)
    assert(Seq(nonRecursive, recursive).sorted == Seq(recursive, nonRecursive))
  }
  they should "not stack overflow" in IO.withTemporaryDirectory { dir =>
    val exact = Glob(dir.toPath.resolve("foo"))
    val exact2 = Glob(dir.toPath.resolve("bar"))
    val exact3 = Glob(dir.toPath.resolve("bar").resolve("baz"))
    val fullFile = sbt.internal.nio.Globs(dir.toPath, true, sbt.io.HiddenFileFilter)
    val fullFile2 = sbt.internal.nio.Globs(dir.toPath / "foo", true, sbt.io.HiddenFileFilter)
    val fullFile3 = sbt.internal.nio.Globs(dir.toPath, true, sbt.io.NothingFilter)
    val scalaS = Glob(dir.toPath / "scala", RecursiveGlob / "*.scala")
    val javaS = Glob(dir.toPath / "java", RecursiveGlob / "*.java")
    val javas2 = Glob(dir.toPath / "scala", RecursiveGlob / "*.java")
    val globs = new java.util.ArrayList(
      Seq(
        RecursiveGlob,
        RecursiveGlob / "foo",
        RecursiveGlob / "foo" / "*.scala",
        scalaS,
        javaS,
        javas2,
        fullFile,
        fullFile3,
        exact,
        fullFile,
        exact2,
        exact3,
        fullFile2
      ).asJava
    )
    1 to 1000 foreach { _ =>
      java.util.Collections.shuffle(globs)
      globs.asScala.sorted
    }
//    assert(
//      Seq(fullFile, exact, fullFile, exact, fullFile).sorted == Seq(
//        exact,
//        exact,
//        fullFile,
//        fullFile,
//        fullFile
//      )
//    )
  }
}
