/*
 * Copyright 2016 Dennis Vriend
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.dnvriend

import fastparse.all.Parsed
import scalaz._
import Scalaz._

object FastParseImplicits {
  implicit class ParserOps[+T](val that: Parsed[T]) extends AnyVal {
    def validation: ValidationNel[String, (T, Int)] = that match {
      case failure: Parsed.Failure =>
        // return the human-readable trace of the parse
        // showing the stack of parsers which were in progress
        // when the parse failed
        // It is a one-line snippet that tells you what the state of the parser was
        // when it failed. It contains the index where the parser was used and the index where the parser failed
        failure.extra.traced.trace.failureNel[(T, Int)]
      case Parsed.Success(value, index) =>
        // index = how much was consumed from the input
        // index = the index in the input string where the parse was performed
        (value, index).successNel[String]
    }
  }

  implicit class ValidationOps[T](val that: ValidationNel[String, (T, Int)]) extends AnyVal {
    def keepType: Validation[NonEmptyList[String], T] = that.map(_._1)
    def keepIndex: Validation[NonEmptyList[String], Int] = that.map(_._2)
  }
}
