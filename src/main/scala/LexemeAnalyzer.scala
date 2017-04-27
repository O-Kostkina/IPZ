import java.io.File

import scala.io.Source
import scala.sys.process.ProcessBuilder.Source

/**
  * Created by kostkinaoksana on 23.03.17.
  */
class LexemeAnalyzer(inFileName: String, outFileName: String) {

  val keywords = Set("PROGRAM", "DEFFUNC", "BEGIN", "END")

  val delimiters = Set('\\', ';', '=', ',')

  val ws = Set(' ', '\n', '\t')

  def parse: List[Lexeme] = {

    def process(input: List[Char], result: List[Lexeme]): List[Lexeme] = {

      def processInteger(input: List[Char], acc: String): (Lexeme, List[Char]) = {
        input match {
          case i :: tail if i.isDigit =>
            processInteger(tail, acc + i)
          case _ =>
            (Constant(0, acc), input)
        }
      }

      def processStringLike(input: List[Char], acc: String): (Lexeme, List[Char]) = {
        input match {
          case i :: tail if i.isLetterOrDigit =>
            processStringLike(tail, acc + i)
          case _ =>
            if (keywords.contains(acc))
              (Keyword(0, acc), input)
            else
              (Identifier(0, acc), input)
        }
      }

      val res = input match {
        case i :: tail if i.isDigit =>
//          println("digit")
          val x = processInteger(input, "")
          Some(x._1) -> x._2

        case i :: tail if i.isLetter =>
//          println("letter")
          val x = processStringLike(input, "")
          Some(x._1) -> x._2

        case i :: tail if delimiters.contains(i) =>
//          println("delim")
          (Some(Delimiter(0, i.toString)), tail)

        case i :: tail if ws.contains(i) =>
//          println("delim")
          None -> tail

        case Nil =>
//          println("eof")
          (None, Nil)
      }
      res match {
        case (Some(lex), lst) => process(lst, lex :: result)
        case (None, Nil) => result.reverse
        case (None, lst) => process(lst, result)
      }
    }


    val inFile = Source.fromFile(new File(inFileName))
    process(inFile.toList, List())

  }

}

object LexemeAnalyzer{
  def apply(inFileName: String, outFileName: String): LexemeAnalyzer = new LexemeAnalyzer(inFileName, outFileName)
}