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

import FastParseImplicits.ParserOps
import fastparse.all._
import fastparse.parsers.Intrinsics.ElemIn

// see: https://www.tutorialspoint.com/scala/scala_regular_expressions.htm
class HaskellPackageParserTest extends TestSpec {
  it should "parse packageName and version using regex" in {
    Hackage.validateHaskellPackage("algebraic:0.1.0.2") should beSuccess(Hackage("algebraic", "0.1.0.2"))
    Hackage.validateHaskellPackage("data-lens:2.11.1") should beSuccess(Hackage("data-lens", "2.11.1"))
  }

  it should "parse packageName and version using Fastparse Parser/Combinator" in {
    val hackagePackage: ElemIn[Char, String] = CharIn('a' to 'z', 'A' to 'Z', "-")
    val hackageVersion: ElemIn[Char, String] = CharIn('0' to '9', ".")
    val hackageParser: Parser[(String, String)] = P(hackagePackage.rep.! ~ ":" ~ hackageVersion.rep.! ~ End)
    hackageParser.parse("data-lens:2.11.1").validation should beSuccess((("data-lens", "2.11.1"), 16))
  }
}
