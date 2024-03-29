package table{

import errors.ExistingKey
import errors.NonExistingKey
import node._

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

        def show_table(): Unit = {
            for ((chave, (node, valor)) <- table) {
                println(s"$chave: ($node , $valor)")
            }
        }
    }

    class FunctionTable(){
        /*  
               Nome da funcao   |    Nó endereço da função    |   Type
            -----------------------------------------------------------------------
                       main               Próprio nó FuncDec          (Int or str)
        */

        def getter(name: String): (Node, String) = {
            FunctionTable._get(name)
        }
        
        def declare(name: String, node: Node, _type: String): Unit = {
            FunctionTable.create(name, node, _type)
        }

        def show_table(): Unit = {
            for ((chave, (node, valor)) <- FunctionTable.table) {
                println(s"$chave: ($node , $valor)")
            }
        }
    }

    object FunctionTable {
        private var table: Map[String, (Node, String)] = Map.empty

        def _get(name: String): (Node, String) = {
            var value : Node  = null
            var _type : String  = ""
            
            if(table.get(name).isDefined){
                var result = table.get(name)
                value = result.get._1
                _type = result.get._2
            }else{
                throw new NonExistingKey(s"Chave não encontrada: $name")
            }
            
            (value,_type)
        }

        def create(name: String, node: Node, _type: String) : Unit = {
            if (!table.contains(name)) {
                table = table + (name -> (node , _type)) 
            } else {
                throw new ExistingKey(s" [FUNCTION TABLE - CREATE] The function $name has already been declared.")
            }
        }

    }
}