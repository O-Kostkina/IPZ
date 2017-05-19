import scala.collection.mutable.HashTable

/**
  * Created by kostkinaoksana on 23.03.17.
  */
trait Lexeme

trait ErrorLexeme extends Lexeme

case class Delimiter(code: Int, literal: String) extends Lexeme
case class Keyword(code: Int, literal: String) extends Lexeme
case class UnsignedInteger(code: Int, literal: String) extends Lexeme
case class Identifier(code: Int, literal: String) extends Lexeme
case class Comment(literal: String) extends Lexeme
case class Email(code: Int, literal: String) extends Lexeme

case class ForbiddenSymbolError(position: Int, line: Int, literal: Char) extends ErrorLexeme
case class UnclosedCommentError(position: Int, line: Int, literal: String) extends ErrorLexeme
case class IllegalIdentifierError(position: Int, line: Int, literal: String) extends ErrorLexeme
case class IllegalDateError(position: Int, line: Int, literal: String) extends ErrorLexeme


trait SyntaxStructure

case class FunctionIdentifier(identifier: Identifier) extends SyntaxStructure
case class ProcedureIdentifier(identifier: Identifier) extends SyntaxStructure
case class Constant(unsignedInteger: UnsignedInteger) extends SyntaxStructure
case class FunctionCharacteristic(ui1: UnsignedInteger, ui2: UnsignedInteger) extends SyntaxStructure
case class Function(functionIdentifier: FunctionIdentifier, constant: Constant, functionCharacteristic: FunctionCharacteristic) extends SyntaxStructure
case class FunctionList(contents: Either[EmptySyntaxStructure.type, (Function, FunctionList)]) extends SyntaxStructure
case class MathFunctionDeclaration(contents: Either[EmptySyntaxStructure.type, FunctionList]) extends SyntaxStructure
case class Declarations(mathFunctionDeclaration: MathFunctionDeclaration) extends SyntaxStructure
case class Block(declarations: Declarations) extends SyntaxStructure
case class Program(procedureIdentifier: ProcedureIdentifier, block: Block) extends SyntaxStructure
case object EmptySyntaxStructure extends SyntaxStructure


trait ErrorSyntax

case class ErrorSyntaxException(msg: String)
