import parser.Parser

object Main {

  def main(args: Array[String]) = {

    if (args.length == 1){
      var parser = new Parser()
      var result = parser.run(args(0))

      println(result)
    }else{
      println("Enter an expression")
    }
  }
}