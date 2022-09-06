/*
    Exoskeleton, version 0.4.0. Copyright 2017-22 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package exoskeleton

import probably.*
import rudiments.*
import gossamer.*
import parasitism.*, monitors.global
import eucalyptus.*, logging.stdout

import unsafeExceptions.canThrowAny

object Tests extends Suite(t"Exoskeleton Tests"):
  def run(using Runner): Unit =
    suite(t"Parsing tests"):
      test(t"read single parameter"):
        Cli2.parse(List(t"one"))
      .assert(_ == Cli2.Params(List(Cli2.Arg(t"one", 0)), Map(), Unset))

      test(t"read several parameters"):
        Cli2.parse(List(t"one", t"two", t"three"))
      .assert(_ == Cli2.Params(List(Cli2.Arg(t"one", 0), Cli2.Arg(t"two", 1), Cli2.Arg(t"three", 2)), Map(), Unset))
      
      test(t"read a flag"):
        Cli2.parse(List(t"-f"))
      .assert(_ == Cli2.Params(Nil, Map(Cli2.Key('f', 0) -> Nil), Unset))
      
      test(t"read a param"):
        Cli2.parse(List(t"-f", t"value"))
      .assert(_ == Cli2.Params(Nil, Map(Cli2.Key('f', 0) -> List(Cli2.Arg(t"value", 1))), Unset))
      
      test(t"read a flag and a param"):
        Cli2.parse(List(t"-q", t"-f", t"value"))
      .assert(_ == Cli2.Params(Nil, Map(Cli2.Key('q', 0) -> Nil, Cli2.Key('f', 1) -> List(Cli2.Arg(t"value", 2))), Unset))
      
      test(t"read arguments, a flag and a param"):
        Cli2.parse(List(t"one", t"-q", t"-f", t"value"))
      .assert(_ == Cli2.Params(List(Cli2.Arg(t"one", 0)), Map(Cli2.Key('q', 1) -> Nil, Cli2.Key('f', 2) -> List(Cli2.Arg(t"value", 3))), Unset))
      
      test(t"read arguments, a flag and a param and unparsed"):
        Cli2.parse(List(t"one", t"-q", t"-f", t"value", t"--", t"abc", t"def"))
      .assert(_ == Cli2.Params(List(Cli2.Arg(t"one", 0)), Map(Cli2.Key('q', 1) -> Nil, Cli2.Key('f', 2) -> List(Cli2.Arg(t"value", 3))), List(Cli2.Arg(t"abc", 5), Cli2.Arg(t"def", 6))))
      
      test(t"read arguments, a flag and a param and empty unparsed"):
        Cli2.parse(List(t"one", t"-q", t"-f", t"value", t"--"))
      .assert(_ == Cli2.Params(List(Cli2.Arg(t"one", 0)), Map(Cli2.Key('q', 1) -> Nil, Cli2.Key('f', 2) -> List(Cli2.Arg(t"value", 3))), Nil))
      
      test(t"read arguments, no params and unparsed"):
        Cli2.parse(List(t"one", t"--", t"unparsed"))
      .assert(_ == Cli2.Params(List(Cli2.Arg(t"one", 0)), Map(), List(Cli2.Arg(t"unparsed", 2))))
      
      test(t"read arguments, no params and unparsed with flags"):
        Cli2.parse(List(t"one", t"--", t"-q"))
      .assert(_ == Cli2.Params(List(Cli2.Arg(t"one", 0)), Map(), List(Cli2.Arg(t"-q", 2))))

      test(t"read arguments, multiple flags"):
        Cli2.parse(List(t"-abc"))
      .assert(_ == Cli2.Params(Nil, Map(Cli2.Key('a', 0) -> Nil, Cli2.Key('b', 0) -> Nil, Cli2.Key('c', 0) -> Nil), Unset))

      test(t"read arguments, multiple flags and combined param"):
        Cli2.parse(List(t"-abc", t"param"))
      .assert(_ == Cli2.Params(Nil, Map(Cli2.Key('a', 0) -> Nil, Cli2.Key('b', 0) -> Nil, Cli2.Key('c', 0) -> List(Cli2.Arg(t"param", 1))), Unset))

      test(t"read a long flag"):
        Cli2.parse(List(t"--abc"))
      .assert(_ == Cli2.Params(Nil, Map(Cli2.Key(t"abc", 0) -> Nil), Unset))

      test(t"read two long flags"):
        Cli2.parse(List(t"--abc", t"--def"))
      .assert(_ == Cli2.Params(Nil, Map(Cli2.Key(t"abc", 0) -> Nil, Cli2.Key(t"def", 1) -> Nil), Unset))

      test(t"read two long params"):
        Cli2.parse(List(t"--abc", t"one", t"--def", t"two"))
      .assert(_ == Cli2.Params(Nil, Map(Cli2.Key(t"abc", 0) -> List(Cli2.Arg(t"one", 1)), Cli2.Key(t"def", 2) -> List(Cli2.Arg(t"two", 3))), Unset))

      test(t"read mixed long and short parameters"):
        Cli2.parse(List(t"-a", t"one", t"--def", t"two"))
      .assert(_ == Cli2.Params(Nil, Map(Cli2.Key('a', 0) -> List(Cli2.Arg(t"one", 1)), Cli2.Key(t"def", 2) -> List(Cli2.Arg(t"two", 3))), Unset))

