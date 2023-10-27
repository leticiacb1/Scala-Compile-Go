package table

import errors.ExistingKey
import errors.NonExistingKey

class SymbolTable (){
    
    // table = { var : (value , type)}
    //O tipo pode ser "INT" ou "STRING" .

    var table : Map[String,(Int, String)] = Map.empty

    def getter(identifier : String): (Int, String) = {
        
        var (value , _type) = 0 , "null"

        if(table.get(identifier).isDefine){
            (value , _type) = table.get(identifier)
        }else{
            throw new NonExistingKey(s"Chave não encontrada: $identifier"))
        }
        
        (value,_type)
    }

    def create(identifier : String , _type: String): Unit  ={
        if(table.get(identifier).isDefine){
            throw new ExistingKey(s"Chave já existente : $identifier")
        }else{
            table = table + (identifier -> (null , _type)) 
        }
    }

    def setter(identifier : String, value : Int) : Unit = {
        table = table + (identifier -> (value, table.get(identifier)[1])
    }
}