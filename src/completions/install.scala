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

import spectacular.*
import gossamer.{where as _, *}
import anticipation.*, filesystemApi.galileiPath
import rudiments.*, homeDirectories.default
import vacuous.*
import serpentine.*, pathHierarchies.unix
import contingency.*
import guillotine.*
import fulminate.*
import turbulence.*
import ambience.*, environments.virtualMachine, systemProperties.virtualMachine
import galilei.*, filesystemOptions.{dereferenceSymlinks, createNonexistent, createNonexistentParents,
    doNotOverwritePreexisting}

enum TabCompletionsInstallation:
  case CommandNotOnPath(script: Text)
  case Shells
      (zsh:  TabCompletionsInstallation.InstallResult,
       bash: TabCompletionsInstallation.InstallResult,
       fish: TabCompletionsInstallation.InstallResult)

object TabCompletionsInstallation:
  given TabCompletionsInstallation is Communicable =
    case CommandNotOnPath(script) =>
      m"The ${script} command is not on the PATH, so completions scripts cannot be installed."

    case Shells(zsh, bash, fish) =>
      m"$zsh\n\n$bash\n\n$fish"

  object InstallResult:
    given InstallResult is Communicable =
      case Installed(shell, path) =>
        m"The $shell completion script installed to $path."

      case AlreadyInstalled(shell, path) =>
        m"A $shell completion script already exists at $path."

      case NoWritableLocation(shell) =>
        m"No writable install location could be found for $shell completions"

      case ShellNotInstalled(shell) =>
        m"The $shell shell is not installed"

  enum InstallResult:
    case Installed(shell: Shell, path: Text)
    case AlreadyInstalled(shell: Shell, path: Text)
    case NoWritableLocation(shell: Shell)
    case ShellNotInstalled(shell: Shell)

object TabCompletions:
  def install(force: Boolean = false)(using service: ShellContext)(using WorkingDirectory, Effectful)
          : TabCompletionsInstallation raises InstallError logs CliEvent =
    tend:
      case PathError(_, _)    => InstallError(InstallError.Reason.Environment)
      case ExecError(_, _, _) => InstallError(InstallError.Reason.Environment)
    .within:
      val scriptPath = sh"sh -c 'command -v ${service.scriptName}'".exec[Text]()
      val command: Text = service.scriptName

      if !force && safely(scriptPath.decodeAs[Path]) != service.script
      then TabCompletionsInstallation.CommandNotOnPath(service.scriptName)
      else
        val zsh: TabCompletionsInstallation.InstallResult =
          if sh"sh -c 'command -v zsh'".exec[ExitStatus]() != ExitStatus.Ok
          then TabCompletionsInstallation.InstallResult.ShellNotInstalled(Shell.Zsh)
          else
            val dirNames = sh"zsh -c 'source ~/.zshrc 2> /dev/null; printf %s, $$fpath'".exec[Text]().cut(t",").to(List)
            val dirs = dirNames.filter(_.trim != t"").map { dir => safely(dir.decodeAs[Path]) }.compact
            install(Shell.Zsh, command, Name(t"_$command"), dirs)

        val bash: TabCompletionsInstallation.InstallResult =
          if sh"sh -c 'command -v bash'".exec[ExitStatus]() != ExitStatus.Ok
          then TabCompletionsInstallation.InstallResult.ShellNotInstalled(Shell.Bash)
          else install(Shell.Bash, command, Name(command), List(Xdg.dataDirs.last / p"bash-completion" /
              p"completions", Xdg.dataHome / p"bash-completion" / p"completions"))

        val fish: TabCompletionsInstallation.InstallResult =
          if sh"sh -c 'command -v fish'".exec[ExitStatus]() != ExitStatus.Ok
          then TabCompletionsInstallation.InstallResult.ShellNotInstalled(Shell.Fish)
          else install(Shell.Fish, command, Name(t"$command.fish"), List(Xdg.dataDirs.last / p"fish" /
              p"vendor_completions.d", Xdg.configHome / p"fish" / p"completions"))

        TabCompletionsInstallation.Shells(zsh, bash, fish)

  def install(shell: Shell, command: Text, scriptName: Name[GeneralForbidden], dirs: List[Path])
      (using Effectful)
          : TabCompletionsInstallation.InstallResult raises InstallError logs CliEvent =

    tend:
      case IoError(_)        => InstallError(InstallError.Reason.Io)
      case OverwriteError(_) => InstallError(InstallError.Reason.Io)
      case StreamError(_)    => InstallError(InstallError.Reason.Io)
    .within:
      dirs.where { dir => dir.exists() && dir.as[Directory].writable() }.let: dir =>
        val path = dir / scriptName
        if path.exists()
        then TabCompletionsInstallation.InstallResult.AlreadyInstalled(shell, path.show)
        else
          script(shell, command).sysBytes.writeTo(path.make[File]())
          TabCompletionsInstallation.InstallResult.Installed(shell, path.show)
      .or(TabCompletionsInstallation.InstallResult.NoWritableLocation(shell))

  // def messages(shell: Shell, global: Boolean): List[Message] =
  //   if shell == Shell.Zsh && !global
  //   then List(m"""Make sure that your ${t"~/.zshrc"} file contains the following lines:

  //     fpath=(~/.zsh/completion $$fpath)
  //     autoload -U compinit
  //     compinit
  //   """)
  //   else Nil

  def script(shell: Shell, command: Text): Text = shell match
    case Shell.Zsh =>
      t"""|#compdef $command
          |local -a ln
          |_$command() {
          |  oldIfs=$$IFS IFS=$$'\t'
          |  $command '{completions}' zsh "$$((CURRENT-1))" "$${#PREFIX}" -- $$words | while read -r -A ln
          |  do
          |    desc=($${ln[1]})
          |    compadd -Q $${ln:1}
          |  done
          |  IFS=$$oldIfs
          |}
          |_$command
          |return 0
          |""".s.stripMargin.tt

    case Shell.Fish =>
      t"""|function completions
          |  set position (count (commandline --tokenize --cut-at-cursor))
          |  ${command} '{completions}' fish $$position (commandline -C -t) -- (commandline -o)
          |end
          |complete -f -c $command -a '(completions)'
          |""".s.stripMargin.tt

    case Shell.Bash =>
      t"""|_${command}_complete() {
          |  output="$$(${command} '{completions}' bash $$COMP_CWORD 0 -- $$COMP_LINE)"
          |  COMPREPLY=($$output)
          |}
          |complete -F _${command}_complete $command
          |""".s.stripMargin.tt

object CliEvent:
  given CliEvent transcribes ExecEvent = CliEvent.Exec(_)

  given CliEvent is Communicable =
    case Exec(event)          => m"exeution error: $event"
    case Installing(location) => m"installing to $location"

enum CliEvent:
  case Exec(event: ExecEvent)
  case Installing(location: Text)
