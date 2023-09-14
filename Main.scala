import parser.Parser
import prepro._

object Main {

  def main(args: Array[String]) = {

    if (args.length == 1){
      var code = PrePro.prePro(args(0))
      
      var parser = new Parser()
      var tree = parser.run(code)

      println(tree.evaluate())
    }else{
      println("Enter an expression")
    }
  }
}