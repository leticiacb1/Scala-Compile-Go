package token

class Token (var _type : String , var _value : Any){

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
        case "." =>  
            "( ." + " , " + " CONCAT " + ")"

        case "EOF" =>  
            "( " + _type + " , " + "EOF " + ")"
        case "(" =>  
            "( PARENTHESES " + " , " + " OPEN_PARENTHESES " + ")"
        case ")" =>  
            "( PARENTHESES " + " , " + " CLOSE_PARENTHESES " + ")"
        case "{" =>  
            "(  " +  _type +  " , " + " OPEN_KEY " + ")"
        case "}" =>  
            "(  " + _type + " , " + " CLOSE_KEY " + ")"
        case ";" =>  
            "(  " + _type + " , " + " SEMICOLON " + ")"
        case "\n" =>  
            "( \\n" + " , " + " END_OF_LINE " + ")"

        case "Println" => 
            "( " + _value + " , " + _type + ")"
        case "if" => 
            "( " + _value + " , " + _type + ")"
        case "else" => 
            "( " + _value + " , " + _type + ")"
        case "for" => 
            "( " + _value + " , " + _type + ")"
        case "Scanln" => 
            "( " + _value + " , " + _type + ")"
        case "var" => 
            "( " + _value + " , " + _type + ")"
        case "int" => 
            "( " + _value + " , " + _type + ")"
        case "string" => 
            "( " + _value + " , " + _type + ")"

        case "IDENTIFIER" => 
            "( " + _value + " , " + _type + ")"

        case "==" => 
            "( " + _type + " , " + "EQUAL_COMPARISON" + ")"
        case ">" => 
            "( " + _type + " , " + "BIGGER_THEN" + ")"
        case "<" => 
            "( " + _type + " , " + "LESS_THEN" + ")"

        case "!" => 
            "( " + _type + " , " + "NOT" + ")"
        case "&&" => 
            "( " + _type + " , " + "AND" + ")"
        case "||" => 
            "( " + _type + " , " + "OR" + ")"
        
        case _     => 
            "( " + _type + " , " + "INVALID " + ")"
    }
  }
}