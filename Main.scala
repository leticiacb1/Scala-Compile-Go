import util.control.Breaks._

class Token (var _type : String , var _value : Int){

  override def toString = "(" + _type + " , " + _value + ")"
}

class Tokenizer ( _source : String){

  var source : String = _source
  var position : Int = 0
  var next : Token = new Token("INIT", 0)

  def selectNext() : Unit = {

    var find_token : Boolean = false;
    var find_invalid : Boolean = false;

    // Definição de valores:
    val EOF = 0
    val INVALID = -1 
    val PLUS  = 1
    val MINUS = 2

    println(" Source é " + source)

    breakable {
    while(true){
      
      
        if(position >= source.length()){
          println(" Maior que o tamanho da string")

            if(next._type != "EOF"){
              next = new Token(_type = "EOF" , _value = EOF)
            }

            break;

        } else {
          
          if (source.charAt(position).isDigit){

            var value_str : String = ""
            println("É digito , " + source.charAt(position))

            breakable {
              while(position < source.length){
                
                if (! source.charAt(position).isDigit){
                  break;
                }else{
                  value_str += source.charAt(position)
                  position += 1
                }
              }
            }

            next = new Token(_type = "INT", _value = value_str.toInt)
            break;

          } else if (source.charAt(position) == '+'){
            println(" Mais ")
            next = new Token(_type = "+", _value = PLUS)
            position += 1
            break;
          
          } else if (source.charAt(position) == '-'){
            
            next = new Token(_type = "-", _value = MINUS)
            position +=1
            break;
          
          } else if (source.charAt(position).isWhitespace){
            position += 1
          
          } else {
            next = new Token(_type = "INVALID", _value = INVALID)
            position += 1
            break;
          }

        } 

      println("Faço algo aqui");

    }
    }
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

  var tokenizer = new Tokenizer("1+2")
  tokenizer.selectNext()
  println("Token : " + tokenizer.next)
  tokenizer.selectNext()
  println(tokenizer.next)
  tokenizer.selectNext()
  println(tokenizer.next)

  // var parser = new Parser(tokenizer)
  // parser.parseExpression()
  // parser.run("Teste")

}
