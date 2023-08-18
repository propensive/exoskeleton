package exoskeleton

import anticipation.*
import rudiments.*
import gossamer.*
import perforate.*
import turbulence.*
import ambience.*

import sun.misc as sm

enum Stdin:
  case Terminal(input: LazyList[Bytes])
  case Pipe(input: LazyList[Bytes])

sealed trait CliContext:
  def args: IArray[Text]
  def environment: Environment
  def workingDirectory: WorkingDirectory

case class CommandLine
    (args: IArray[Text], currentArg: Int, argPosition: Int, environment: Environment,
        workingDirectory: WorkingDirectory)
extends CliContext

case class Invocation
    (args: IArray[Text], environment: Environment, workingDirectory: WorkingDirectory, stdin: () => Stdin,
        stdout: LazyList[Bytes] => Unit, stderr: LazyList[Bytes] => Unit)
extends CliContext:
  def signals(signals: Signal*): LazyList[Signal] = 
    val funnel: Funnel[Signal] = Funnel()
    
    signals.foreach: signal =>
      sm.Signal.handle(sm.Signal(signal.shortName.s), event => funnel.put(signal))
    
    funnel.stream

abstract class Application:
  protected given environment(using invocation: Invocation): Environment = invocation.environment
  protected given workingDirectory(using invocation: Invocation): WorkingDirectory = invocation.workingDirectory
  
  protected given stdio(using invocation: Invocation): Stdio with
    def putErrBytes(bytes: Bytes): Unit = System.err.nn.write(bytes.mutable(using Unsafe))
    def putOutBytes(bytes: Bytes): Unit = System.out.nn.write(bytes.mutable(using Unsafe))
    def putErrText(text: Text): Unit = System.err.nn.print(text.s)
    def putOutText(text: Text): Unit = System.out.nn.print(text.s)
  
  def invoke(using CliContext): Execution

  def main(args: IArray[Text]): Unit =
    import systemProperties.jvm
    def in: LazyList[Bytes] = safely(System.in.nn.stream[Bytes]).or(LazyList())
    
    def stdin(): Stdin = safely(Properties.exoskeleton.stdin[Text]()) match
      case t"term" => Stdin.Terminal(in)
      case _       => Stdin.Pipe(in)
    
    def stdout(stream: LazyList[Bytes]): Unit = stream.foreach: bytes =>
      System.out.nn.write(bytes.mutable(using Unsafe))
    
    def stderr(stream: LazyList[Bytes]): Unit = stream.foreach: bytes =>
      System.err.nn.write(bytes.mutable(using Unsafe))

    val invocation = Invocation(args, environments.jvm, workingDirectories.default, () => stdin(), stdout(_),
        stderr(_))
    
    invoke(using invocation).execute(invocation) match
      case ExitStatus.Ok           => System.exit(0)
      case ExitStatus.Fail(status) => System.exit(1)

case class Execution(execute: Invocation => ExitStatus)

def execute(block: Invocation ?=> ExitStatus): Execution = Execution(block(using _))

