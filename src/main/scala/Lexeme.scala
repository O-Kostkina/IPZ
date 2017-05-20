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


trait ErrorSyntax

case class ErrorSyntaxException(msg: String) extends ErrorSyntax

case object EmptySyntaxStructure extends SyntaxStructure {
  override def toString: String = "EmptyLexeme"
}


case class Program(procedureIdentifier: ProcedureIdentifier, block: Block) extends SyntaxStructure {
  override def toString: String = {
    s"""
       #Program -> Keyword -> PROGRAM
       # |   |  -> ProcedureIdentifier -> ${procedureIdentifier.identifier}
       # |   |  -> Delimeter -> ;
       # |
       # V
       #$block
    """.stripMargin('#')
  }
}

case class Block(declarations: Declarations) extends SyntaxStructure {
  override def toString: String = {
    s"""
       #Block -> Keyword -> BEGIN
       # |  | -> StatementsList -> ${EmptySyntaxStructure.toString}
       # |  | -> Keyword -> END
       # |
       # V
       #${declarations.toString}
    """.stripMargin('#')
  }
}

case class Declarations(mathFunctionDeclaration: MathFunctionDeclaration) extends SyntaxStructure {
  override def toString: String = {
    s"""
       #Declarations
       # |
       # V
       #${mathFunctionDeclaration.toString}
    """.stripMargin('#')
  }
}

case class MathFunctionDeclaration(contents: Either[EmptySyntaxStructure.type, FunctionList]) extends SyntaxStructure {
  override def toString: String = {
    contents match {
      case Left(e) =>
        s"MathFunctionDeclaration -> ${e.toString}"
      case Right(c) =>
        s"""
           #MathFunctionDeclaration -> Keyword -> DEFFUNC
           # |
           # V
           #${c.toString}
    """.stripMargin('#')
    }
  }
}

case class FunctionList(contents: Either[EmptySyntaxStructure.type, (Function, FunctionList)]) extends SyntaxStructure {
  override def toString: String = {
    contents match {
      case Left(e) => s"FunctionList -> ${e.toString}"
      case Right((f, fl)) =>
        s"""
           #FunctionList --> Function -->  FunctionIdentifier -> ${f.functionIdentifier}
           # |                    |   -->  Delimeter -> =
           # |                    |   -->  Constant -> ${f.constant.unsignedInteger}
           # |                    |   -->  Delimeter -> \\
           # |                    |   -->  FunctionCharacteristic --> ${f.functionCharacteristic.ui1}
           # |                    |   -->  Delimeter --> ,
           # |                                                  | --> ${f.functionCharacteristic.ui2}
           # |                    |   -->  Delimeter -> ;
           # V
           #${fl.toString}
        """.stripMargin('#')

    }

  }


}



