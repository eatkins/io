package sbt.io

import org.scalatest.FlatSpec
import syntax._

class GlobAsFilterSpec extends FlatSpec {
  "GlobAsFilter" should "work with simple files" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "file")
    val nestedFile = new File(new File(dir, "subdir"), "subdir-file")
    val filter = dir.toGlob.toFilter
    assert(filter.accept(dir))
    assert(!filter.accept(file))
    assert(!filter.accept(nestedFile))
    assert(!dir.toGlob.withFilter(NothingFilter).toFilter.accept(dir))
  }
  it should "work with globs" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "file")
    val nestedFile = new File(new File(dir, "subdir"), "subdir-file")
    val glob = dir * AllPassFilter
    assert(!glob.toFilter.accept(dir))
    assert(glob.toFilter.accept(file))
    assert(!glob.toFilter.accept(nestedFile))
    val nothingGlob = dir * NothingFilter
    Seq(dir, file, nestedFile).foreach(f => assert(!nothingGlob.toFilter.accept(f)))
  }
  it should "work with recursive globs" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "file")
    val nestedFile = new File(new File(dir, "subdir"), "subdir-file")
    val glob = dir ** AllPassFilter
    assert(!glob.toFilter.accept(dir))
    assert(glob.toFilter.accept(file))
    assert(glob.toFilter.accept(nestedFile))
    val nothingGlob = dir ** NothingFilter
    Seq(dir, file, nestedFile).foreach(f => assert(!nothingGlob.toFilter.accept(f)))
  }
  it should "work with traversable globs" in IO.withTemporaryDirectory { dir =>
    val file = new File(dir, "file")
    val subdir = new File(dir, "subdir")
    val nestedFile = new File(new File(dir, "subdir"), "subdir-file")
    val globs = Seq(dir * AllPassFilter, subdir * AllPassFilter)
    val filter = globs.toFilter
    assert(!filter.accept(dir))
    assert(filter.accept(file))
    assert(filter.accept(subdir))
    assert(filter.accept(nestedFile))
  }
}
