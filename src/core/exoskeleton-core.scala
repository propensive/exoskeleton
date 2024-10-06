/*
    Exoskeleton, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import profanity.*
import rudiments.*
import gossamer.*
import digression.*
import hieroglyph.*, textMetrics.uniform
import escapade.*
import ambience.*
import anticipation.*
import turbulence.*

import sun.misc as sm

package unhandledErrors:
  given UnhandledErrorHandler as silent:
    def handle(block: => Exit)(using Stdio): Exit =
      try block catch
        case error: Exception => Exit(1)
        case error: Throwable => Exit(2)

  given UnhandledErrorHandler as genericErrorMessage:
    def handle(block: => Exit)(using Stdio): Exit = try block catch
      case error: Exception =>
        Out.println(t"An unexpected error occurred.")
        Exit(1)

      case error: Throwable =>
        Out.println(t"An unexpected error occurred.")
        Exit(2)

  given UnhandledErrorHandler as exceptionMessage:
    def handle(block: => Exit)(using Stdio): Exit = try block catch
      case error: Exception =>
        Out.println(error.toString.tt)
        Exit(1)

      case error: Throwable =>
        Out.println(error.toString.tt)
        Exit(2)

  given UnhandledErrorHandler as stackTrace:
    def handle(block: => Exit)(using Stdio): Exit = try block catch
      case error: Exception =>
        Out.println(StackTrace(error).teletype)
        Exit(1)

      case error: Throwable =>
        Out.println(StackTrace(error).teletype)
        Exit(2)

package executives:
  given direct(using handler: UnhandledErrorHandler): Executive with
    type Return = Exit
    type CliType = CliInvocation

    def cli
        (arguments:        Iterable[Text],
         environment:      Environment,
         workingDirectory: WorkingDirectory,
         stdio:            Stdio,
         signals:          Spool[Signal])
        (using interpreter: CliInterpreter)
            : CliInvocation =

      CliInvocation(Cli.arguments(arguments), environments.virtualMachine, workingDirectories.default, stdio, signals)

    def process(cli: CliInvocation)(exitStatus: CliType ?=> Exit): Exit =
      handler.handle(exitStatus(using cli))(using cli.stdio)

def application(using executive: Executive, interpreter: CliInterpreter)
    (arguments: Iterable[Text], signals: List[Signal] = Nil)
    (block: Cli ?=> executive.Return)
        : Unit =

  val spool: Spool[Signal] = Spool()
  signals.each { signal => sm.Signal.handle(sm.Signal(signal.shortName.s), event => spool.put(signal)) }

  // FIXME: We shouldn't assume so much about the STDIO. Instead, we should check the environment variables
  val cli = executive.cli(arguments, environments.virtualMachine, workingDirectories.default, stdioSources.virtualMachine.ansi, spool)

  System.exit(executive.process(cli)(block)())
