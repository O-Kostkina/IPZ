/**
  * Created by kostkinaoksana on 23.03.17.
  */
trait Lexeme

trait ErrorLexeme extends Lexeme

case class Delimiter(code: Int, literal: String) extends Lexeme
case class MultiDelimiter(code: Int, literal: String) extends Lexeme
case class Keyword(code: Int, literal: String) extends Lexeme
case class Constant(code: Int, literal: String) extends Lexeme
case class Identifier(code: Int, literal: String) extends Lexeme
case class Comment(literal: String) extends Lexeme
case class Telephone(literal: String) extends Lexeme
case class DateLexeme(literal: String) extends Lexeme

case class ForbiddenSymbolError(position: Int, literal: Char) extends ErrorLexeme
case class UnclosedCommentError(position: Int, literal: String) extends ErrorLexeme
case class IllegalIdentifierError(position: Int, literal: String) extends ErrorLexeme
case class IllegalDateError(position: Int, literal: String) extends ErrorLexeme
