import util.control.Breaks._

class MyCustomException(s: String) extends Exception(s) {}  

class Token (var _type : String , var _value : Int){

  override def toString  : String = {
  
    if(_type == "INT"){
      "( " + _value + " , " + _type + ")"
    } else if (_type == "+"){
      "( " + _type + " , " + "PLUS " + ")"
    } else if (_type == "-") {
      "( " + _type + " , " + "MINUS " + ")"
    } else if (_type == "EOF"){
      "( " + _type + " , " + "EOF " + ")"
    } else {
      "( " + _type + " , " + "INVALID " + ")"
    }

  }
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

class Parser() {
  
  def parseExpression(tokenizer : Tokenizer) : Int = {
    println("Parser Expresion")

    var result : Int =  0

    if(tokenizer.next._type == "INT"){
      result = tokenizer.next._value
      tokenizer.selectNext()
      println(" Inteiro ")
    }else{
      throw new MyCustomException("Tem que começar com numero") 
    }

    result
  }

  def run(source_code : String) : Int = {
    
    var tokenizer  = new Tokenizer(source_code)
    tokenizer.selectNext()

    println(tokenizer.next)

    var result = parseExpression(tokenizer)
    println("Chamou o result")
    result
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
  
  var parser = new Parser()
  parser.run(" 1 + 2")
}
