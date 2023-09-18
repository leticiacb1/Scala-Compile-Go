import parser.Parser
import scala.io.Source
import prepro._

// Para passar o conteudo do arquivo para o programa rodar:
// scala Main < meuarquivo.go

object Main {

  def load_file() : String = {
    val input = Source.stdin
    val linhas = input.getLines()

    var source_code = ""

    for (linha <- linhas) {
      source_code = source_code + " " + linha
    }

    input.close()
    source_code
  }

  def main(args: Array[String]) = {

    var source_code = load_file()
    var code = PrePro.prePro(source_code)
    
    var parser = new Parser()
    var tree = parser.run(code)

    println(tree.evaluate())

  }
}