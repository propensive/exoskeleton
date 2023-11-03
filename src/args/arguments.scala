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

import rudiments.*
import ambience.*
import anticipation.*
import spectacular.*
import gossamer.*
import profanity.*

object Shell:
  given decoder: Decoder[Shell] = text => valueOf(text.lower.capitalize.s)
  given encoder: Encoder[Shell] = _.toString.tt.lower

enum Shell:
  case Zsh, Bash, Fish

object SimpleParameterInterpreter extends CliInterpreter[List[Argument]]:
  def apply(arguments: List[Argument])(using Cli): List[Argument] = arguments

object Cli:
  def arguments
     (textArguments: Iterable[Text], focus: Maybe[Int] = Unset, position: Maybe[Int] = Unset)
     : List[Argument] =

    textArguments.to(List).zipWithIndex.map: (text, index) =>
      Argument(index, text, if focus == index then position else Unset)

trait Cli extends ProcessContext:
  type State
  
  private var currentState: State = initialState
  protected def initialState: State
  
  def apply(): State = currentState
  
  def update(state: State): Unit = synchronized:
    currentState = state
  
  def arguments: List[Argument]
  def environment: Environment
  def workingDirectory: WorkingDirectory

  def register(flag: Flag[?]): Unit = ()
  def present(flag: Flag[?]): Unit = ()
  def unknown(argument: Argument): Unit = ()

trait CliInterpreter[ParametersType]:
  def apply(arguments: List[Argument])(using Cli): ParametersType

object Parameters:
  def apply[ParametersType: CliInterpreter](arguments: List[Argument])(using Cli): ParametersType =
    summon[CliInterpreter[ParametersType]](arguments)

case class Argument(position: Int, value: Text, cursor: Maybe[Int]):
  def apply(): Text = value
  def prefix: Maybe[Text] = cursor.mm(value.take(_))
  def suffix: Maybe[Text] = cursor.mm(value.drop(_))

package parameterInterpretation:
  given simple: SimpleParameterInterpreter.type = SimpleParameterInterpreter

def arguments(using cli: Cli): List[Argument] = cli.arguments

def parameters
    [ParametersType]
    (using interpreter: CliInterpreter[ParametersType], cli: Cli)
    : ParametersType =
  interpreter(arguments)