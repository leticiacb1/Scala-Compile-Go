package table

class SymbolTable (){
    var table = Map[String,Int]

    def getter(identifier : String): Int = {
        table.get(identifier)
    }

    def setter(identifier : String, value : Int) : Unit = {
        table = table + (identifier -> value)
    }
}