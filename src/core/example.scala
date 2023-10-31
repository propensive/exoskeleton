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
import profanity.*
import rudiments.*
import turbulence.*

@main
def fury(): Unit =
  import parameterInterpretation.simple
  import errorHandlers.throwUnsafely
  Daemon.listen:
    parameters.headOption.foreach: arg =>
      arg.suggest(List(
        Suggestion(t"en", t"English"),
        Suggestion(t"fr", t"French"),
        Suggestion(t"de", t"German"),
      ))
    
    parameters.lift(1).foreach: arg =>
      parameters(0)() match
        case t"fr" =>
          arg.suggest(List(
            Suggestion(t"un", t"The first number"),
            Suggestion(t"deux", t"The smallest prime"),
            Suggestion(t"trois", t"The second prime"),
            Suggestion(t"quatre", t"Two squared"),
            Suggestion(t"cinq", t"Half of ten"),
            Suggestion(t"six", t"Three times two"),
            Suggestion(t"sept", t"The fourth prime number"),
            Suggestion(t"huit", t"Two to the power of three"),
            Suggestion(t"neuf", t"Three squared"),
            Suggestion(t"dix", t"Five times two"),
          ))
          
        case t"en" =>
          arg.suggest(List(
            Suggestion(t"one", t"The first number"),
            Suggestion(t"two", t"The smallest prime"),
            Suggestion(t"three", t"The second prime"),
            Suggestion(t"four", t"Two squared"),
            Suggestion(t"five", t"Half of ten"),
            Suggestion(t"six", t"Three times two"),
            Suggestion(t"seven", t"The fourth prime number"),
            Suggestion(t"eight", t"Two to the power of three"),
            Suggestion(t"nine", t"Three squared"),
            Suggestion(t"ten", t"Five times two"),
          ))
    
        case _ =>
          arg.suggest(List(
            Suggestion(t"ein", t"The first number"),
            Suggestion(t"zwei", t"The smallest prime"),
            Suggestion(t"drei", t"The second prime"),
            Suggestion(t"vier", t"Two squared"),
            Suggestion(t"fünf", t"Half of ten"),
            Suggestion(t"sechs", t"Three times two"),
            Suggestion(t"sieben", t"The fourth prime number"),
            Suggestion(t"acht", t"Two to the power of three"),
            Suggestion(t"neun", t"Three squared"),
            Suggestion(t"zehn", t"Five times two"),
          ))
    
    execute:
      Out.println(arguments.debug)
      supervise:
        terminal:
          println(tty.mode.debug)
          ExitStatus.Ok