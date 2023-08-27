import scala.util.control.Breaks._

class Token (var _type : String , var _value : Int){

  override def toString = "(" + _type + " , " + _value + ")"
}

class Tokenizer ( _source : String ,  _position : Int ,  _next : Token ){

  var position : Int = 0
  var source : String = "1 + 2"
  var next : Token = _next

  def selectNext() : Unit = {

    var find_token : Boolean = false;
    var find_invalid : Boolean = false;

    // Definição de valores:
    val EOF = 0
    val INVALID = -1 
    val PLUS  = 1
    val MINUS = 2

    while(true){
      
      if(position >= source.length()){
        println(" Maior que o tamanho da string")

        if(next._type != "EOF"){
          next = new Token(_type = "EOF" , _value = EOF)
        }
      } else {

        if (source.charAt(_position).isDigit){
          println(" Primeiro elemento é um dígito")
        }

      }

      println("Aqui");
      break;
    } 

    println("Faço algo aqui");
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
  var other_token = new Token("Teste2" , 2)
  
  println(other_token)
  println(token)

  var tokenizer = new Tokenizer("Teste" , 0 , token)
  tokenizer.selectNext()

  var parser = new Parser(tokenizer)
  parser.parseExpression()
  parser.run("Teste")

}
