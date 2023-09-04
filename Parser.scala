package parser

import errors.InvalidExpression
import tokenizer.Tokenizer
import util.control.Breaks._
import constants._

class Parser() {

  def parserFactory(tokenizer : Tokenizer) : Int = {

      if (tokenizer.next._type ==  Types.INT) {
        var num_value = tokenizer.next._value
        tokenizer.selectNext()
        num_value
      }

      else if (tokenizer.next._type ==  Types.PLUS.toString) {
        tokenizer.selectNext()
        var result = parserFactory(tokenizer)
        (1)*result
      } 

      else if (tokenizer.next._type ==  Types.MINUS.toString) {
        tokenizer.selectNext()
        var result = parserFactory(tokenizer)
        (-1)*result
      } 



      else if (tokenizer.next._type == Types.OPEN_PARENTHESES.toString) {
        tokenizer.selectNext()
        var result = parserExpression(tokenizer)

        if(tokenizer.next._type == Types.CLOSE_PARENTHESES.toString){
          tokenizer.selectNext()
          result
        }else{
          throw new InvalidExpression("\n Expected close parentheses type | Got " + tokenizer.next)
        }

      }

      else {
        throw new InvalidExpression("\n Unexpected value in factory | Got " + tokenizer.next)
      }
    }
  
  def parserTerm(tokenizer : Tokenizer) : Int = {
     
    var operators = List(Types.TIMES.toString , Types.BAR.toString )
    var result : Int = parserFactory(tokenizer)

    breakable {
      while(true){
  
        if(! operators.contains(tokenizer.next._type)){
          break;
        }

        if(tokenizer.next._type == Types.TIMES.toString){
          tokenizer.selectNext()
          result *= parserFactory(tokenizer)
        }

        if(tokenizer.next._type == Types.BAR.toString){
          tokenizer.selectNext()
          result /= parserFactory(tokenizer)
        }
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