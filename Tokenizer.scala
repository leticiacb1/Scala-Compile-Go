package tokenizer

import util.control.Breaks._
import token.Token
import constants._

class Tokenizer ( _source : String){

  var source : String = _source
  var position : Int = 0
  var next : Token = new Token("", 0)
  var reserved_words : Map[String, Map[String, Any]] = Map(
    "println" -> Map("type" -> Types.PRINTLN , "value" -> Values.PRINTLN),
  )

  def selectNext() : Unit = {

    var find_token : Boolean = false;
    var find_invalid : Boolean = false;
    
    breakable {
        while(true){
        
            if(position >= source.length()){
                if(next._type != Types.EOF){ 
                    next = new Token(_type = Types.EOF , _value = Values.EOF)
                }

                break;
            } else {
                
                source.charAt(position) match {

                    case x if x.isDigit => {
                        var value_str : String = ""
    
                        breakable {
                            while(position < source.length){
                                
                                if (source.charAt(position).isDigit){
                                    value_str += source.charAt(position)
                                    position += 1
                                }else{
                                    break;
                                }
                            }
                        }
                        
                        next = new Token(_type = Types.INT, _value = value_str.toInt)
                        break;
                    }

                    case Types.PLUS => {
                        next = new Token(_type = Types.PLUS.toString , _value = Values.PLUS)
                        position += 1
                        break;
                    }

                    case Types.MINUS => {
                        next = new Token(_type = Types.MINUS.toString , _value = Values.MINUS)
                        position +=1
                        break;
                    }

                    case Types.TIMES => {
                        next = new Token(_type = Types.TIMES.toString , _value = Values.TIMES)
                        position += 1
                        break;
                    }

                    case Types.BAR => {
                        next = new Token(_type = Types.BAR.toString , _value = Values.BAR)
                        position +=1
                        break;
                    }

                    case Types.EQUAL => {
                        next = new Token(_type = Types.EQUAL.toString , _value = Values.EQUAL)
                        position +=1
                        break;
                    }

                    case Types.OPEN_PARENTHESES => {
                        next = new Token(_type = Types.OPEN_PARENTHESES.toString , _value = Values.PARENTHESES)
                        position +=1
                        break;
                    }

                    case Types.CLOSE_PARENTHESES => {
                        next = new Token(_type = Types.CLOSE_PARENTHESES.toString , _value = Values.PARENTHESES)
                        position +=1
                        break;
                    }

                    case Types.END_OF_LINE => {
                        next = new Token(_type = Types.END_OF_LINE.toString , _value = Values.END_OF_LINE)
                        position +=1
                        break;
                    }

                    case c if c.forall(_.isLetter) => {
                        var value_str : String = ""
                        val pattern: Regex = "[a-zA-Z0-9_]+".r

                        breakable {
                            while((position < source.length) && (pattern.matches(c))){
                                value_str += c
                                position += 1
                            }
                        }

                        if (reserved_words.contains(value_str)){
                            next = new Token(_type = reserved_words("println")("type") , _value = reserved_words("println")("value"))
                        }else{
                            next = new Token(_type = Types.IDENTIFIER , _value = value_str)
                        }

                        break;
                    }

                    case c if c.isWhitespace => {
                        position += 1
                    }

                    case _ => {
                        next = new Token(_type = Types.INVALID , _value = Values.INVALID)
                        position += 1
                        break;
                    }
                }                
            } 
        }
    }
  }

}