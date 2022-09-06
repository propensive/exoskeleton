package exoskeleton

import rudiments.*
import gossamer.*

object Cli2:
  trait Part:
    def index: Int
    def text: Text
  
  case class Arg(text: Text, index: Int) extends Part
  
  case class Separator(index: Int) extends Part:
    def text: Text = t"--"
  
  case class Key(value: Text | Char, index: Int) extends Part:
    def text = value match
      case char: Char => t"-$char"
      case text: Text => t"--$text"

  case class Params(args: List[Arg], params: Map[Key, List[Arg]], unparsed: Maybe[List[Arg]]):
    private lazy val all: IArray[Part] =
      val parsed = (args ++ params.values.flatten ++ params.keys).sortBy(_.index)
      val unparsed2 = unparsed.option.map(Separator(parsed.last.index) :: _).getOrElse(Nil)
      IArray.from(parsed ++ unparsed2)
    
    def apply(idx: Int): Part = all(idx)

  def parse(args: List[Text]): Params =
    @tailrec
    def recur(idx: Int, todo: List[Text], args: List[Arg], acc: ListMap[Key, List[Arg]]): Params =
      todo match
        case Nil =>
          Params(args.reverse, acc.view.mapValues(_.reverse).to(Map), Unset)
        case t"--" :: tail =>
          Params(args.reverse, acc.view.mapValues(_.reverse).to(Map), tail.zipWithIndex.map { (k, v) => Arg(k, v + idx + 1) })
        case arg :: tail =>
          if arg.startsWith(t"--") then recur(idx + 1, tail, args, acc.updated(Key(arg.drop(2), idx), Nil))
          else if arg.startsWith(t"-") && arg != t"-" then
            val acc2 = arg.drop(1).chars.foldLeft(acc): (acc, next) =>
              acc.updated(Key(next, idx), Nil)
            recur(idx + 1, tail, args, acc2)
          else if acc.isEmpty then recur(idx + 1, tail, Arg(arg, idx) :: args, acc)
          else recur(idx + 1, tail, args, acc.updated(acc.last(0), Arg(arg, idx) :: acc.last(1)))

    recur(0, args, Nil, ListMap())

