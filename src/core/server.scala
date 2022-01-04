package exoskeleton

import jovian.*
import gossamer.*
import rudiments.*

import scala.concurrent.*
import scala.util.*

import drains.stdout
import encodings.Utf8

import unsafeExceptions.canThrowAny

case class CommandLine(args: List[Text], env: Map[Text, Text], scriptName: Text, scriptDir: Maybe[Unix.Directory],
                           stdin: () => DataStream, stdout: DataStream => Unit, exit: Int => Unit)
extends Drain:
  def write(msg: Text): Unit = stdout(LazyList(msg.bytes))
  
  def pwd: Unix.Directory throws PwdError =
    env.get(t"PWD").flatMap(Unix.parse(_).map(_.directory)).getOrElse:
      throw PwdError()

trait App:
  def main(using CommandLine): ExitStatus

trait ServerApp() extends App:
  final def main(args: IArray[Text]): Unit =
    args.to(List) match
      case t"::start::" :: scriptName :: fifo :: Int(pid) :: Nil =>
        val runnable: Runnable = () => server(scriptName, fifo, pid)
        Thread(runnable, "exoskeleton-dispatcher").start()
      
      case args =>
        val env = System.getenv.nn.asScala.map(_.nn.show -> _.nn.show).to(Map)
        val systemStdin = Option(System.in).getOrElse(sys.exit(10)).nn
        def stdin(): DataStream = Util.readInputStream(systemStdin, 1.mb)
        def stdout(ds: DataStream): Unit = ds.map(_.unsafeMutable).foreach(System.out.nn.writeBytes(_))
        val commandLine = CommandLine(args, env, t"java", Unset, () => stdin(), stdout, sys.exit(_))

        main(using commandLine)()

  def server(scriptName: Text, fifo: Text, serverPid: Int): Unit =
    val socket: Unix.IoPath = Unix.parse(fifo).getOrElse(sys.exit(11))

    Runtime.getRuntime.nn.addShutdownHook:
      val runnable: Runnable = () => try socket.file.delete() catch case e: Exception => ()
      Thread(runnable, "exoskeleton-cleanup")

    case class AppInstance(pid: Int, scriptName: Text = t"", scriptDir: Maybe[Text] = Unset,
                               args: List[Text] = Nil, env: Map[Text, Text] = Map(),
                               runDir: Maybe[Unix.Directory] = Unset):

      val terminate: Promise[Unit] = Promise()

      def stop(): Unit = terminate.complete(Success(()))
      
      def spawn(map: Map[Int, AppInstance]): Map[Int, AppInstance] =
        val runnable: Runnable = () =>
          try
            val fifoIn = (runDir.otherwise(sys.exit(12)) / t"$scriptName-$pid.stdin.sock").file
            val fifoOut = Unix.Fifo((runDir.otherwise(sys.exit(13)) / t"$scriptName-$pid.stdout.sock").file)
            val terminate = Promise[Int]()
            val commandLine = CommandLine(args, env, scriptName, scriptDir.option.flatMap(Unix.parse(_)).map(_.directory).maybe, () => fifoIn.read[DataStream](1.mb),
                _.writeTo(fifoOut), exit => terminate.complete(util.Success(exit)))

            val exit = main(using commandLine)
            fifoOut.close()
            val exitFile = (runDir.otherwise(sys.exit(14)) / t"$scriptName-$pid.exit").file
            exit().show.bytes.writeTo(exitFile)
          catch case NonFatal(err) =>
            try
              //fifoOut.close()
              val exitFile = (runDir.otherwise(sys.exit(14)) / t"$scriptName-$pid.exit").file
              ExitStatus.Fail(2).show.bytes.writeTo(exitFile)
            catch case _: Throwable => sys.exit(2)
        
        val thread = Thread(runnable, "exoskeleton-spawner")
        thread.start()
        map
    
    def parseEnv(env: List[Text]): Map[Text, Text] = env.flatMap:
      _.cut(t"=", 2).to(List) match
        case List(key, value) => List(key -> value)
        case _                => Nil
    .to(Map)
    
    socket.file.read[LazyList[Line]](256.kb).foldLeft(Map[Int, AppInstance]()):
      case (map, line) =>
        line.text.cut(t"\t").to(List) match
          case t"PROCESS" :: Int(pid) :: _ =>
            map.updated(pid, AppInstance(pid))
          
          case t"RUNDIR" :: Int(pid) :: dir :: _ =>
            Unix.parse(dir).fold(map):
              dir => map.updated(pid, map(pid).copy(runDir = dir.directory))
          
          case t"SCRIPT" :: Int(pid) :: script :: _ =>
            map.updated(pid, map(pid).copy(scriptName = script))

          case t"SCRIPTDIR" :: Int(pid) :: scriptDir :: _ =>
            map.updated(pid, map(pid).copy(scriptDir = scriptDir))
          
          case t"ARGS" :: Int(pid) :: Int(count) :: args =>
            map.updated(pid, map(pid).copy(args = args.take(count)))
          
          case t"ENV" :: Int(pid) :: env =>
            map.updated(pid, map(pid).copy(env = parseEnv(env)))
          
          case t"START" :: Int(pid) :: _ =>
            map(pid).spawn(map)
          
          case t"STOP" :: Int(pid) :: _ =>
            map(pid).stop()
            map
          
          case t"SHUTDOWN" :: _ =>
            sys.exit(0)
            map
          
          case msg =>
            Out.println(t"Unexpected message: ${msg.toString}")
            map
