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
        case "EOF" =>  
            "( " + _type + " , " + "EOF " + ")"
        case _     => 
            "( " + _type + " , " + "INVALID " + ")"
    }
  }
}