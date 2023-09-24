package token

class Token (var _type : String , var _value : Int){

  override def toString  : String = {
  
    _type match {
        case "INT" => 
            "( " + _value + " , " + _type + ")"
        case "+"   => 
            "( " + _type + " , " + "PLUS " + ")"
        case "-"   => 
            "( " + _type + " , " + "MINUS " + ")"
        case "*"   => 
            "( " + _type + " , " + "TIMES " + ")"
        case "/"   => 
            "( " + _type + " , " + "BAR " + ")"
        case "=" =>  
            "( " + _type  + " , " + " EQUAL " + ")"
        case "EOF" =>  
            "( " + _type + " , " + "EOF " + ")"
        case "(" =>  
            "( PARENTHESES " + " , " + " OPEN_PARENTHESES " + ")"
        case ")" =>  
            "( PARENTHESES " + " , " + " CLOSE_PARENTHESES " + ")"
        case "\n" =>  
            "( \\n" + " , " + " END_OF_LINE " + ")"
        case "PRINTLN" => 
            "( " + _value + " , " + _type + ")"
        case "IDENTIFIER" => 
            "( " + _value + " , " + _type + ")"
        case _     => 
            "( " + _type + " , " + "INVALID " + ")"
    }
  }
}