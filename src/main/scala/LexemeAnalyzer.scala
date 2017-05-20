import java.io.File

import scala.collection.mutable
import scala.io.Source
import scala.sys.process.ProcessBuilder.Source
import scala.util.Try

/**
  * Created by kostkinaoksana on 23.03.17.
  */
class LexemeAnalyzer(inFileName: String) {

  import LexemeAnalyzer._

  private val keywords = Map("PROGRAM" -> 401, "DEFFUNC" -> 402, "BEGIN" -> 403, "END" -> 404)

  private val letters = Set('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')

  private val delimiters = Set('\\', ';', '=', ',', '+')

  private var identifierCount = 1000

  private var emailCount = 2000

  private var constantCount = 500

  private val identifiers = mutable.Map[String, Int]()

  private val emails = mutable.Map[String, Int]()

  private val constants = mutable.Map[String, Int]()

  private val ws = Set(' ', '\n', '\t')

  private var line: Int = 1

  def parse: ParseResult = {

    def process(input: List[Char], result: List[Lexeme], pos: Int): List[Lexeme] = {

      def processIllegalIdentifier(input: List[Char], acc: String, pos: Int): (Lexeme, List[Char], Int) = {
        input match {
          case i :: tail if !(ws.contains(i) || delimiters.contains(i)) =>
            processIllegalIdentifier(tail, acc + i, pos + 1)
          case _ =>
            (IllegalIdentifierError(pos, line, acc ), input, pos)
        }
      }

      def processInteger(input: List[Char], acc: String, pos: Int): (Lexeme, List[Char], Int) = {
        input match {
          case i :: tail if i.isDigit =>
            processInteger(tail, acc + i, pos + 1)
          case i :: tail if ws.contains(i) || delimiters.contains(i) =>
            constantCount += 1
            constants.update(acc, constantCount)
            (UnsignedInteger(constantCount, acc), input, pos)


          case _ =>
            processIllegalIdentifier(acc.toList ++ input, "", pos - acc.length)
        }
      }

      def processComment(input: List[Char], acc: String, pos: Int): (Lexeme, List[Char], Int) = {
        input match {
          case ')' :: tail if acc.last == '*' =>
            (Comment(acc :+ ')'), tail, pos + 1)

          case i :: tail if acc == "(" && i != '*' =>
            (ForbiddenSymbolError(pos, line, '('), input, pos)

          case i :: Nil =>
            (UnclosedCommentError(pos, line, acc + i), Nil, pos + 1)

          case i :: tail =>
            if (i == '\n') line += 1
            processComment(tail, acc + i, 1)

        }
      }

      def processStringLike(input: List[Char], acc: String, pos: Int): (Lexeme, List[Char], Int) = {
        input match {
          case i :: tail if letters.contains(i) || i.isDigit =>
            processStringLike(tail, acc + i, pos + 1)
          case i :: tail if i == '@' =>
            processEmail(acc.toList ++ input, "", pos - acc.length)
          case _ =>
            keywords.get(acc) match {
              case Some(code) => (Keyword(code, acc), input, pos)
              case None =>
                identifiers.get(acc) match {
                  case Some(code) => (Identifier(code, acc), input, pos)
                  case None =>
                    identifierCount += 1
                    identifiers.update(acc, identifierCount)
                    (Identifier(identifierCount, acc), input, pos)
                }
            }


        }
      }

      def processEmail(input: List[Char], acc: String, pos: Int): (Lexeme, List[Char], Int) = {
        input match {

          case i :: tail  if (i == '@' && (acc.isEmpty || acc.contains('.'))) || (i == '.' && (!acc.last.isLetter || (!acc.contains('@') || acc.contains('.')))) || (i.isDigit && (acc.contains('@') || acc.isEmpty)) || (acc.contains('.') && acc.lastIndexOf('.') < acc.length - 4)  =>
            processIllegalIdentifier(acc.toList ++ input, "", pos - acc.length)

          case i :: tail if (ws.contains(i) || delimiters.contains(i)) && (acc.lastIndexOf('.') == acc.length - 4) =>
            emails.get(acc) match {
              case Some(code) => (Email(code, acc), input, pos)
              case None =>
                emailCount += 1
                emails.update(acc, identifierCount)
                (Email(emailCount, acc), input, pos)
            }

          case i :: tail if i.isLetterOrDigit || i == '@' || i == '.'  =>
            processEmail(tail, acc + i, pos + 1)

          case _ =>
            (IllegalIdentifierError(pos, line, acc), input, pos)


        }
      }



      val res = input match {
        case i :: tail if i.isDigit =>
          val x = processInteger(input, "", pos)
          Some(x._1) -> (x._2, x._3)

        case i :: tail if letters.contains(i) =>
          val x = processStringLike(input, "", pos)
          Some(x._1) -> (x._2, x._3)

        case i :: tail if delimiters.contains(i) =>
          (Some(Delimiter(i.toInt, i.toString)), (tail, pos + 1))

        case i :: tail if ws.contains(i) =>
          if (i == '\n') line += 1
          None -> (tail, 1)

        case '(' :: tail =>
          val x = processComment(input, "", pos)
          Some(x._1) -> (x._2, x._3)

        case i :: tail =>
          Some(ForbiddenSymbolError(pos, line, i)) -> (tail, pos + 1)

        case Nil =>
          (None, (Nil, pos + 1))

      }
      res match {
        case (Some(lex), (lst, p)) => process(lst, lex :: result, p)
        case (None, (Nil, p)) => result.reverse
        case (None, (lst, p)) => process(lst, result, p)
      }
    }


    val inFile = Source.fromFile(new File(inFileName))
    val lexemes = process(inFile.toList, List(), 0)

    val table = lexemes.filter(_.isInstanceOf[Delimiter]).map(_.asInstanceOf[Delimiter]).map(c => (c.code, c.literal)).distinct.sortBy(_._1) ++
      lexemes.filter(_.isInstanceOf[Keyword]).map(_.asInstanceOf[Keyword]).map(c => c.code -> c.literal).distinct.sortBy(_._1) ++
      lexemes.filter(_.isInstanceOf[UnsignedInteger]).map(_.asInstanceOf[UnsignedInteger]).map(c => c.code -> c.literal).distinct.sortBy(_._1) ++
      lexemes.filter(_.isInstanceOf[Identifier]).map(_.asInstanceOf[Identifier]).map(c => c.code -> c.literal).distinct.sortBy(_._1)

    ParseResult(lexemes.filterNot(_.isInstanceOf[Comment]).filterNot(_.isInstanceOf[ErrorLexeme]), table, lexemes.filter(_.isInstanceOf[ErrorLexeme]).map(_.asInstanceOf[ErrorLexeme]))
  }

}

object LexemeAnalyzer {

  case class ParseResult(lexemes: Seq[Lexeme], table: Seq[(Int, String)], errors: Seq[ErrorLexeme])

  def apply(inFileName: String): LexemeAnalyzer = new LexemeAnalyzer(inFileName)
}