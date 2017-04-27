/**
  * Created by kostkinaoksana on 23.03.17.
  */
sealed trait Lexeme

case class Delimiter(code: Int, literal: String) extends Lexeme
case class MultiDelimiter(code: Int, literal: String) extends Lexeme
case class Keyword(code: Int, literal: String) extends Lexeme
case class Constant(code: Int, literal: String) extends Lexeme
case class Identifier(code: Int, literal: String) extends Lexeme
case class WhiteSpace(code: Int, literal: String) extends Lexeme
case class ErrorLexeme(literal: Int) extends Lexeme
