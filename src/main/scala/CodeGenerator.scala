/**
  * Created by kostkinaoksana on 5/26/17.
  */

object CodeGenerator {
  case class SemanticError(message: String)

  def apply(program: Program): CodeGenerator = new CodeGenerator(program)
}

class CodeGenerator(program: Program) {
 import CodeGenerator._

  def generateAsm: Either[SemanticError, String] = {

    def generateProgram(program: Program): Either[SemanticError, String] = {

      generateDeclarations(program.block.declarations.mathFunctionDeclaration).map { decl =>
        s"""
           |data segment
           |
           |$decl
           |data ends
           |
           |code segment
           |start:
           |assume cs:code, ds: data
           |nop
           |code ends
           |
        |end start
      """.stripMargin
      }
    }

    def generateDeclarations(declarations: MathFunctionDeclaration): Either[SemanticError, String] = {
      declarations.contents match {
        case Left(EmptySyntaxStructure) => Right("")
        case Right(fl) =>
          generateFunctionList(fl, Set())
      }
    }

    def generateFunctionList(functionList: FunctionList, names: Set[String]): Either[SemanticError, String] = {
      functionList.contents match {
        case Left(EmptySyntaxStructure) => Right("")
        case Right((func, fl)) =>
          val name = func.functionIdentifier.identifier.literal

          val start = func.functionCharacteristic.ui1.literal.toInt
          val end = func.functionCharacteristic.ui2.literal.toInt
          val step = (end - start) / func.constant.unsignedInteger.literal.toInt

          if(names.contains(name)){
            Left(SemanticError(s"Dublicate funtion name '$name'"))
          }else if(end < start){
            Left(SemanticError(s"Final value less than initial in function $name"))
          }else if(step > end - start){
            Left(SemanticError(s"Step is larger than final value - initial in function $name"))
          }else{
            generateFunctionList(fl, names + name).map{funcListCode =>
              s"$name db ${(start until end by step).mkString(", ")}\n" + funcListCode
            }
          }

      }
    }

    generateProgram(program)

  }

}
