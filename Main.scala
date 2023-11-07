import parser.Parser
import scala.io.Source
import prepro._
import table.SymbolTable
import constants._
import assembler.Assembler

object Main {

  def load_file(fileName: String) : List[String] = {
   try {
      val source = Source.fromFile(fileName)
      val content = source.getLines().map(_ + "\n").toList
      
      source.close()
      content
    } catch {
      case e: Exception =>
      println("\n [LOAD FILE] Fail to load file | " + e.getMessage)
      List("Exception erro.")
    }
  }

  def main(args: Array[String]) : Unit = {
    val fileName = args(0)
    var source_code = load_file(fileName)
    var code = PrePro.prePro(source_code)
    
    var parser = new Parser()
    var tree = parser.run(code)

    var symbol_table = new SymbolTable()
    println("\n--- COMEÇA EVALUATE ---\n")
    tree.evaluate(symbol_table)

    var asm = new Assembler()
    asm.mount(fileName)
  }
}