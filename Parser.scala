package parser

import errors.InvalidExpression
import tokenizer.Tokenizer
import util.control.Breaks._
import constants._

class Parser() {
  
  def parseExpression(tokenizer : Tokenizer) : Int = {
    var result : Int =  0
    var operators = List("+", "-")

    if(tokenizer.next._type == "INT"){
      result = tokenizer.next._value
      tokenizer.selectNext()
      
      if(! operators.contains(tokenizer.next._type)){
        throw new InvalidExpression("Espera-se um operador pós numero") 
      }

      breakable {
        while(true){

          if(tokenizer.next._type == "+"){
            tokenizer.selectNext()
            
            tokenizer.next._type match {
                case "INT" => { result += tokenizer.next._value }
                case _     => { throw new InvalidExpression("Espera-se um número pós operador") }
            }
          }

          if(tokenizer.next._type == "-"){
            tokenizer.selectNext()

            tokenizer.next._type match {
                case "INT" => { result -= tokenizer.next._value }
                case _     => { throw new InvalidExpression("Espera-se um número pós operador") }
            }
          }

          tokenizer.selectNext()
          if(! operators.contains(tokenizer.next._type)){
            break;
          }

        }
      }
      
      result

    }else{
      throw new InvalidExpression("Tem que começar com numero") 
    }
  }

  def run(source_code : String) : Int = {
    
    var tokenizer  = new Tokenizer(source_code)
    tokenizer.selectNext()

    var result = parseExpression(tokenizer)

    if(tokenizer.next._type != "EOF"){
      throw new InvalidExpression("O ultimo token deve ser do tipo EOF")
    }

    result
  }

}