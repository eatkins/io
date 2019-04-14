/*
 * sbt IO
 *
 * Copyright 2011 - 2019, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

package sbt.internal.nio

import com.swoval.functional.{ Either => SEither }

/**
 * Utilities for converting between swoval and sbt data types.
 */
private[nio] object SwovalConverters {

  implicit class RangeOps(val range: (Int, Int)) extends AnyVal {
    def toSwovalDepth: Int = range._2 match {
      case Int.MaxValue => Int.MaxValue
      case d            => d - 1
    }
  }
  implicit class SwovalEitherOps[L, R](val either: SEither[L, R]) extends AnyVal {
    def asScala[R0](implicit f: R => R0): Either[L, R0] = either match {
      case l: com.swoval.functional.Either.Left[L, R] =>
        Left(com.swoval.functional.Either.leftProjection(l).getValue)
      case r: com.swoval.functional.Either.Right[L, R] => Right(f(r.get()))
    }
  }
}