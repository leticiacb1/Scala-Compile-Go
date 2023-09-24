package table

class SymbolTable (){
    var table : Map[String,Int]

    def getter(identifier : String): Int = {
        table.getOrElse(identifier, throw new NoSuchElementException(s"Chave não encontrada: $identifier"))
    }

    def setter(identifier : String, value : Int) : Unit = {
        table = table + (identifier -> value)
    }
}