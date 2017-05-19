/**
  * Created by kostkinaoksana on 5/19/17.
  */


class SyntaxAnalyzer(lexemes: List[Lexeme]) {

  def analyze = {

    def findProgram(input: List[Lexeme]): Either[Program, ErrorSyntax] = {
      input match {
        case Keyword(_, "PROGRAM") :: (i @ Identifier(_, _)) :: Delimiter(_,";") :: tail =>
          Program(ProcedureIdentifier(i), findBlock(tail))

        case _ =>

      }
    }

  }

}
