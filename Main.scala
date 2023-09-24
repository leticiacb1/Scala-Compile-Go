import parser.Parser
import scala.io.Source
import prepro._

// Para passar o conteudo do arquivo para o programa rodar:
// scala Main < meuarquivo.go

object Main {

  def load_file(fileName: String) : String = {
   try {
      val source = Source.fromFile(fileName)
      val content = source.getLines().mkString("\n") // Concatena as linhas em uma única string
      source.close()
      content
    } catch {
      case e: Exception =>
        println("Ocorreu um erro ao ler o arquivo: " + e.getMessage)
        ""
    }
  }

  def main(args: Array[String]) = {
    val fileName = args(0)
    var source_code = load_file(fileName)
    var code = PrePro.prePro(source_code)
    
    var parser = new Parser()
    var tree = parser.run(code)

    tree.evaluate()
  }
}