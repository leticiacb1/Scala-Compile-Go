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

    breakable {
    while(true){
      
      
        if(position >= source.length()){
            if(next._type != "EOF"){
              next = new Token(_type = "EOF" , _value = EOF)
            }

            break;

        } else {
          
          if (source.charAt(position).isDigit){

            var value_str : String = ""
            
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
    }
    }
  }

}

class Parser() {
  
  def parseExpression(tokenizer : Tokenizer) : Int = {
    var result : Int =  0

    if(tokenizer.next._type == "INT"){
      result = tokenizer.next._value
      tokenizer.selectNext()
      
      var operators = List("+", "-")
      if(! operators.contains(tokenizer.next._type)){
        throw new MyCustomException("Espera-se um operador pós numero") 
      }

      breakable {
        while(true){
          if(tokenizer.next._type == "+"){
            tokenizer.selectNext()
            
            if(tokenizer.next._type == "INT"){
              result += tokenizer.next._value
            }else if (tokenizer.next._type == "INVALID"){
              throw new MyCustomException("Caracter inválido")
            }else{
              throw new MyCustomException("Espera-se um número pós operador")
            }

          }

          if(tokenizer.next._type == "-"){
            tokenizer.selectNext()
            
            if(tokenizer.next._type == "INT"){
              result -= tokenizer.next._value
            }else if (tokenizer.next._type == "INVALID"){
              throw new MyCustomException("Caracter inválido")
            }else{
              throw new MyCustomException("Espera-se um número pós operador")
            }
          }

          // Próximo token
          tokenizer.selectNext()

          // Caso o próximo não seja outro operador
          if(! operators.contains(tokenizer.next._type)){
            break;
          }
        }
      }

      // Verficar se foi encontrado operador inválido:
      if(tokenizer.next._type == "INVALID"){
        throw new MyCustomException("Caracter inválido")
      } else {
        result
      }

    }else{
      throw new MyCustomException("Tem que começar com numero") 
    }
  }

  def run(source_code : String) : Int = {
    
    var tokenizer  = new Tokenizer(source_code)
    tokenizer.selectNext()

    var result = parseExpression(tokenizer)

    if(tokenizer.next._type != "EOF"){
      throw new MyCustomException("Algo de errado aconteceu")
    }

    result
  }

}

object Main extends App {

  if (args.length == 1){
        var parser = new Parser()
        var result = parser.run(args(0))

        println("Resultado: " + result)
  }else{
        println("I didn't get info.")
  }

}