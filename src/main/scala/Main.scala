/**
  * Created by kostkinaoksana on 22.03.17.
  */
object Main {

  def main(args : Array[String]) = {
    val res = LexemeAnalyzer("src/main/resources/program.txt").parse
    println("Lexemes:")
    res.lexemes.foreach(println)
    println("Table:")
    res.table.foreach(el => println(s"${el._1}: ${el._2}"))
    println("Errors:")
    res.errors.foreach(println)
  }

}
