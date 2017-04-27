/**
  * Created by kostkinaoksana on 22.03.17.
  */
object Main {

  def main(args : Array[String]) = {
    LexemeAnalyzer("src/main/resources/program.txt", "result.txt").parse.foreach(println)

  }

}
