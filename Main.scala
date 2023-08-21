
class Token (var _type : String , var _value : Int)

class Tokenizer (var _source : String , var _position : Int , var _next : Token ){

  def selectNext() : Unit = {
    println("Faço algo aqui")
  } 
}

class Parser(var _tokenizer : Tokenizer) {
  
  var tokenizer = _tokenizer

  def parseExpression() : Unit = {
    println("Algo aqui")
  }

  def run(_code : String) : Unit = {
    println("Algo aqui")
  }

}

object Main extends App {

  if (args.length == 1)
        println(s"Hello, ${args(0)}")
  else
        println("I didn't get your name.")

  var token = new Token("Teste" , 1)
  println(token._type)

  var tokenizer = new Tokenizer("Teste" , 0 , token)
  tokenizer.selectNext()

  var parser = new Parser(tokenizer)
  parser.parseExpression()
  parser.run("Teste")

}
