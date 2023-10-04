import constants._
import table.SymbolTable

package node {
    abstract class Node (val _value : Any){
        
        var children : List[Node] = Nil

        def add_child(child : Node) : Unit = {
            children = children :+ child
        }

        def evaluate(symbol_table: SymbolTable) : Int

        override def toString () : String = {
            "<Node(value = " + _value +")>"
        }
    }
}

package binop {
    import node._
    class BinOp(_value : Any) extends Node(_value){

        def evaluate(symbol_table: SymbolTable) : Int =  { 
            _value match {

                case Types.PLUS => {
                    children(0).evaluate(symbol_table) + children(1).evaluate(symbol_table)  
                }

                case Types.MINUS => {
                    children(0).evaluate(symbol_table) - children(1).evaluate(symbol_table)  
                } 

                case Types.BAR => { 
                    children(0).evaluate(symbol_table) / children(1).evaluate(symbol_table)
                }

                case Types.TIMES => {
                    children(0).evaluate(symbol_table) * children(1).evaluate(symbol_table)
                }

                case _ => {throw new Exception("Error de tipo")}
            }
        }
    }

}

package intval {
    import node._
    class IntVal(_value : Any) extends Node (_value){
        def evaluate(symbol_table: SymbolTable) : Int =  { 
            _value.asInstanceOf[Int]
        }
    }
}

package unop {
    import node._
    class UnOp(_value : Any) extends Node (_value){
        def evaluate(symbol_table: SymbolTable) : Int =  { 
            _value match {

                case Types.PLUS => {
                    children(0).evaluate(symbol_table)
                }

                case Types.MINUS => {
                    -children(0).evaluate(symbol_table)
                }
                case _  => {throw new Exception("Error de tipo")}
            }
        }
    }
}

package noop {
    import node._
    class NoOp(_value : Any) extends Node (_value){
        def evaluate(symbol_table: SymbolTable) : Int =  { 
            0
        }
    }
}

package block {
    import node._
    class Block(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : Int =  { 
            for (child <- children) {
                child.evaluate(symbol_table)
            }
            0
        }
    }
}


package assigment {
    import node._
    class Assigment(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : Int =  { 
            // Não deve retornar nada
            var result = children(1).evaluate(symbol_table)
            symbol_table.setter(children(0)._value.toString , result)
            0
        }
    }
}

package functions {
    import node._
    class Println(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : Int =  { 
            var expression_result = children(0).evaluate(symbol_table)
            println(expression_result)
            0
        }
    }

    class If(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : Int =  { 
            var conditional = children(0)
            var block_if    = children(1) 

            if(conditional.evaluate(symbol_table)){
                block_if.evaluate(symbol_table)
            }else if(children.size > 2){

                if(! conditional.evaluate(symbol_table)){
                    children(2).evaluate(symbol_table)
                }
            }
        }
    }
}

package identifier {
    import node._
    class Identifier(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : Int =  { 
            symbol_table.getter(_value.toString)
        }
    }
}