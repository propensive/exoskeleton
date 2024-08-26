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

import rudiments.*
import vacuous.*
import escapade.*
import anticipation.*

import language.experimental.pureFunctions

object Suggestion:
  def apply
      (text: Text,
       description: Optional[Text | Teletype],
       hidden: Boolean = false,
       incomplete: Boolean = false,
       aliases: List[Text] = Nil)
          : Suggestion =

    new Suggestion(text, description, hidden, incomplete, aliases)

case class Suggestion
    (text:        Text,
     description: Optional[Text | Teletype],
     hidden:      Boolean,
     incomplete:  Boolean,
     aliases:     List[Text])