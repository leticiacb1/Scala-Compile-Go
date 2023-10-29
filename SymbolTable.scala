package table

import errors.ExistingKey
import errors.NonExistingKey

class SymbolTable (){
    
    // table = { var : (value , type)}
    //O tipo pode ser "INT" ou "STRING" .

    var table : Map[String,(Any, String)] = Map.empty

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

    def create(identifier : String , _type: String): Unit  ={
        if(table.get(identifier).isDefined){
            throw new ExistingKey(s"Chave já existente : $identifier")
        }else{
            table = table + (identifier -> (0 , _type)) 
        }
    }

    def setter(identifier : String, value : Any) : Unit = {
        var _type = table.get(identifier).get._2
        table = table + (identifier -> (value, _type))
    }
}