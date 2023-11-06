package table

import errors.ExistingKey
import errors.NonExistingKey
import scala.languageFeature.postfixOps

class SymbolTable (){
    
    // table = { var : (value , type , position)}
    //O tipo pode ser "INT" ou "STRING" .

    var table : Map[String,(Any, String, Int)] = Map.empty
    var position : Int = 0

    def getter(identifier : String): (Any, String) = {
        var value : Any  = ""
        var _type : String  = ""

        if(table.get(identifier).isDefined){
            var result = table.get(identifier)
            value = result.get._1
            _type = result.get._2
        }else{
            throw new NonExistingKey(s"Chave não encontrada: $identifier")
        }
        
        (value,_type)
    }

    def get_position(identifier : String) : Int = {
        var result = table.get(identifier)
        var position = result.get._3
        return position
    }

    def create(identifier : String , _type: String): Unit  ={
        if(table.get(identifier).isDefined){
            throw new ExistingKey(s"Chave já existente : $identifier")
        }else{
            table = table + (identifier -> (0 , _type , 4 + position*4)) 
            position += 1
        }
    }

    def setter(identifier : String, value : Any) : Unit = {
        var _type = table.get(identifier).get._2
        var _position = table.get(identifier).get._3
        table = table + (identifier -> (value, _type, _position))
    }
}