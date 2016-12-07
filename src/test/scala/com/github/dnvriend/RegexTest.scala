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

// see: https://www.tutorialspoint.com/scala/scala_regular_expressions.htm
class RegexTest extends TestSpec {
  it should "match the Hackage Resource format" in {
    Hackage.validateHaskellPackage("algebraic:0.1.0.2") should beSuccess(Hackage("algebraic", "0.1.0.2"))
  }
}
