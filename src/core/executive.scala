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
import vacuous.*
import gossamer.*
import fulminate.*
import digression.*
import hieroglyph.*, textMetrics.uniform
import escapade.*
import ambience.*
import galilei.*
import anticipation.*
import turbulence.*

import sun.misc as sm

//import language.experimental.captureChecking

trait Executive:
  type Return
  type CliType <: Cli

  def cli
      (fullArguments:    Iterable[Text],
       environment:      Environment,
       workingDirectory: WorkingDirectory,
       stdio:            Stdio,
       signals:          Spool[Signal])
      (using interpreter: CliInterpreter)
          : CliType

  def process(cli: CliType)(result: CliType ?=> Return): ExitStatus

trait UnhandledErrorHandler:
  def handle(block: => ExitStatus)(using Stdio): ExitStatus

package unhandledErrors:
  given silent: UnhandledErrorHandler with
    def handle(block: => ExitStatus)(using Stdio): ExitStatus =
      try block catch
        case error: Exception => ExitStatus(1)
        case error: Throwable => ExitStatus(2)

  given genericErrorMessage: UnhandledErrorHandler with
    def handle(block: => ExitStatus)(using Stdio): ExitStatus = try block catch
      case error: Exception =>
        Out.println(t"An unexpected error occurred.")
        ExitStatus(1)

      case error: Throwable =>
        Out.println(t"An unexpected error occurred.")
        ExitStatus(2)

  given exceptionMessage: UnhandledErrorHandler with
    def handle(block: => ExitStatus)(using Stdio): ExitStatus = try block catch
      case error: Exception =>
        Out.println(error.toString.tt)
        ExitStatus(1)

      case error: Throwable =>
        Out.println(error.toString.tt)
        ExitStatus(2)

  given stackTrace: UnhandledErrorHandler with
    def handle(block: => ExitStatus)(using Stdio): ExitStatus = try block catch
      case error: Exception =>
        Out.println(StackTrace(error).teletype)
        ExitStatus(1)

      case error: Throwable =>
        Out.println(StackTrace(error).teletype)
        ExitStatus(2)

package executives:
  given direct(using handler: UnhandledErrorHandler): Executive with
    type Return = ExitStatus
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

    def process(cli: CliInvocation)(exitStatus: CliType ?=> ExitStatus): ExitStatus =
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

case class CliInvocation
    (arguments:        List[Argument],
     environment:      Environment,
     workingDirectory: WorkingDirectory,
     stdio:            Stdio,
     signals:          Spool[Signal])
    (using interpreter: CliInterpreter)
extends Cli, Stdio:

  export stdio.{termcap, out, err, in}

  private lazy val parameters: interpreter.Parameters = interpreter.interpret(arguments)

  def readParameter[OperandType](flag: Flag[OperandType])(using FlagInterpreter[OperandType], Suggestions[OperandType]): Optional[OperandType] =
    given Cli = this
    parameters.read(flag)

trait ShellContext:
  def scriptName: Text
  def script: Path

@capability
erased trait Effectful

object InstallError:
  object Reason:
    given Reason is Communicable as communicable =
      case Environment => m"it was not possible to get enough information about the install environment"
      case Io          => m"an I/O error occurred when trying to write an installation file"

  enum Reason:
    case Environment, Io

case class InstallError(reason: InstallError.Reason) extends Error(m"the installation failed because $reason")
