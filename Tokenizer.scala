package tokenizer

import util.control.Breaks._
import token.Token
import constants._

class Tokenizer ( _source : String){

  var source : String = _source
  var position : Int = 0
  var next : Token = new Token("", 0)

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

                    case x if x.isWhitespace => {
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