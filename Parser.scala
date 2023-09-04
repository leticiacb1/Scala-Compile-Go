package parser

import errors.InvalidExpression
import tokenizer.Tokenizer
import util.control.Breaks._
import constants._

class Parser() {

  def parserFactory() : Int = {

    if(tokenizer.next._type == "INT"){
      
    }
    tokenizer.next._type match {

      case "INT" => {
        var num_value = Parser().tokenizer.next._value
        tokenizer.selectNext()
        num_value
      }

      case Types.PLUS => {
        tokenizer.selectNext()
        result = parserFactory()
        (1)*result
      } 

      case Types.MINUS => {
        tokenizer.selectNext()
        result = parserFactory()
        (-1)*result
      } 

      case Types.OPEN_PARENTHESES => {
        tokenizer.selectNext()
        result = parserExpression()

        if(tokenizer.next._type == Types.CLOSE_PARENTHESESES){
          tokenizer.selectNext()
          result
        }else{
          throw new InvalidExpression("\n Expected close parentheses type | Got " + tokenizer.next)
        }

      }

      case _ => {
        throw new InvalidExpression("\n Unexpected value in factory | Got " + tokenizer.next)
      }
  }
  
  def parserTerm(tokenizer : Tokenizer) : Int = {
    var result : Int =  0
    var operators = List(Types.TIMES.toString , Types.BAR.toString )

    if(tokenizer.next._type != "INT"){
      throw new InvalidExpression("\n Expected number type | Got " + tokenizer.next)
    }

    result = tokenizer.next._value
    tokenizer.selectNext()
    
    breakable {
      while(true){

        if(! operators.contains(tokenizer.next._type)){
          break;
        }

        if(tokenizer.next._type == Types.TIMES.toString){
          tokenizer.selectNext()
          
          tokenizer.next._type match {
              case "INT" => { result *= tokenizer.next._value }
              case _     => { throw new InvalidExpression("\n Expected number type | Got " + tokenizer.next)}
          }
        }

        if(tokenizer.next._type == Types.BAR.toString){
          tokenizer.selectNext()

          tokenizer.next._type match {
              case "INT" => { result /= tokenizer.next._value }
              case _     => { throw new InvalidExpression("\n Expected number type | Got " + tokenizer.next) }
          }
        }

        tokenizer.selectNext()

      }
    }
    
    result
  }

  def parserExpression(tokenizer : Tokenizer) : Int = {
    var result = parserTerm(tokenizer)
    var operators = List(Types.PLUS.toString, Types.MINUS.toString)

    breakable {
      while(true){
  
        if(! operators.contains(tokenizer.next._type)){
          break;
        }

        if(tokenizer.next._type == Types.MINUS.toString){
          tokenizer.selectNext()
          result -= parserTerm(tokenizer)
        }

        if(tokenizer.next._type == Types.PLUS.toString){
          tokenizer.selectNext()
          result += parserTerm(tokenizer)
        }
      }
    }

    result
  }

  def run(source_code : String) : Int = {
    
    var tokenizer  = new Tokenizer(source_code)
    tokenizer.selectNext()

    var result = parserExpression(tokenizer)
    
    if(tokenizer.next._type != Types.EOF ){
      throw new InvalidExpression("\n Expected EOF type | Got " + tokenizer.next)
    }

    result
  }

}