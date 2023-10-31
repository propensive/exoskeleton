/*
    Exoskeleton, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import perforate.*
import spectacular.*
import parasite.*
import gossamer.*
import escapade.*
import profanity.*
import rudiments.*
import turbulence.*

@main
def fury(): Unit =
  import parameterInterpretation.simple
  import errorHandlers.throwUnsafely
  Daemon.listen:
    parameters.headOption.foreach: arg =>
      arg.suggest(List(Suggestion(t"yes", out"Definitely so"), Suggestion(t"no", out"By $Bold(no) means")))
    
    parameters.lift(1).foreach: arg =>
      arg.suggest(List(Suggestion(t"1", t"one"), Suggestion(t"2", t"two"), Suggestion(t"3", t"three")))

    execute:
      Out.println(arguments.debug)
      supervise:
        terminal:
          println(tty.mode.debug)
          ExitStatus.Ok
