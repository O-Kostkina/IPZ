import scala.util.Try

/**
  * Created by kostkinaoksana on 5/19/17.
  */

object SyntaxAnalyzer {
  def apply(lexemes: Seq[Lexeme]): SyntaxAnalyzer = new SyntaxAnalyzer(lexemes)
}

class SyntaxAnalyzer(lexemes: Seq[Lexeme]) {

  def analyze: Either[ErrorSyntax, Program] = {

    def findProgram(input: Seq[Lexeme]): Either[ErrorSyntax, Program] = {
      input match {
        case Keyword(_, _, _, "PROGRAM") +: (i@Identifier(_, _, _, _)) +: Delimiter(_, _, _, ";") +: tail =>
          findBlock(tail).map(x => Program(ProcedureIdentifier(i), x))

        case _ =>
          Left(ErrorSyntaxException(s"Syntax error at program declaration at line  ${Lexeme.line(input.head)}"))

      }
    }

    def findBlock(input: Seq[Lexeme]): Either[ErrorSyntax, Block] = {
      input match {
        case declaration :+ Keyword(_, _, _, "BEGIN") :+ Keyword(_, _, _, "END") =>
          findDeclarations(declaration).map(x => Block(x))

        case _ =>
          Left(ErrorSyntaxException(s"Unexpected position of BEGIN and END at line ${Lexeme.line(input.head)}"))
      }
    }

    def findDeclarations(input: Seq[Lexeme]): Either[ErrorSyntax, Declarations] = {
      findMathFunctionDeclaration(input).map(x => Declarations(x))
    }

    def findMathFunctionDeclaration(input: Seq[Lexeme]): Either[ErrorSyntax, MathFunctionDeclaration] = {
      input match {
        case Keyword(_, _, _, "DEFFUNC") +: tail =>
          findFunctionList(tail).map(x => MathFunctionDeclaration(Right(x)))

        case Nil =>
          Right(MathFunctionDeclaration(Left(EmptySyntaxStructure)))

        case _ =>
          Left(ErrorSyntaxException(s"Syntax error at function list declaration at line  ${Lexeme.line(input.head)}"))
      }
    }

    def findFunctionList(input: Seq[Lexeme]): Either[ErrorSyntax, FunctionList] = {
      input match {
        case Nil =>
          Right(FunctionList(Left(EmptySyntaxStructure)))

        case _ =>
          findFunction(input) match {
            case (Left(e), _) => Left(e)
            case (Right(f), tail) =>
              findFunctionList(tail).map(fl => FunctionList(Right((f, fl))))
          }
      }


    }

    def findFunction(input: Seq[Lexeme]): (Either[ErrorSyntax, Function], Seq[Lexeme]) = {
      input.splitAt(input.find(x => Try(x.asInstanceOf[Delimiter].literal == ";").toOption.contains(true)).map(input.indexOf(_)).getOrElse(input.length - 1) + 1) match {
        case ((i: Identifier) +: Delimiter(_, _, _, "=") +: (ui: UnsignedInteger) +: funcChar :+ Delimiter(_, _, 59, ";"), tail) =>
          (findFunctionChar(funcChar).map(fc => Function(FunctionIdentifier(i), Constant(ui), fc)), tail)
        case (_, tail) =>
          (Left(ErrorSyntaxException(s"Syntax error at function declaration at line ${Lexeme.line(input.head)}")), tail)
      }

    }

    def findFunctionChar(input: Seq[Lexeme]): Either[ErrorSyntax, FunctionCharacteristic] = {
      input match {
        case Delimiter(_, _, _, "\\") +: (ui1: UnsignedInteger) +: Delimiter(_, _, _, ",") +: (ui2: UnsignedInteger) +: Nil =>
          Right(FunctionCharacteristic(ui1, ui2))

        case _ =>
          Left(ErrorSyntaxException(s"Syntax error at function characteristic declaration at line ${Lexeme.line(input.head)}"))
      }
    }


    findProgram(lexemes)

  }

}
