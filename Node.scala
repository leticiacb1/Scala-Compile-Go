import constants._
import table.SymbolTable

package node {
    abstract class Node (val _value : Any){
        
        var children : List[Node] = Nil

        def add_child(child : Node) : Unit = {
            children = children :+ child
        }

        def evaluate(symbol_table: SymbolTable) : Int

    }
}

package binop {
    import node._
    class BinOp(_value : Any) extends Node(_value){

        def evaluate(symbol_table: SymbolTable) : Int =  { 
            _value match {

                case Types.PLUS => {
                    children(0).evaluate() + children(1).evaluate()  
                }

                case Types.MINUS => {
                    children(0).evaluate() - children(1).evaluate()  
                } 

                case Types.BAR => { 
                    children(0).evaluate() / children(1).evaluate()
                }

                case Types.TIMES => {
                    children(0).evaluate() * children(1).evaluate()
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
                    children(0).evaluate()
                }

                case Types.MINUS => {
                    -children(0).evaluate()
                }
                case _  => {throw new Exception("Error de tipo")}
            }
        }
    }
}

package noop {
    import node._
    class NoOp(var _value : Any) extends Node (_value){
        def evaluate(symbol_table: SymbolTable) : Unit =  { 
            None
        }
    }
}

package block {
    import node._
    class Block(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : Unit =  { 
            for (child <- children) {
                child.evaluate(symbol_table)
            }
        }
    }
}


package assigment {
    import node._
    class Assigment(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : Unit =  { 
            // Não deve retornar nada
            var result = children(1).evaluate(symbol_table)
            symbol_table.setter(children(0)._value , result)
        }
    }
}

package functions {
    import node._
    class Println(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : Unit =  { 
            var expression_result = children(0).evaluate(symbol_table)
            println(expression_result)
        }
    }
}

package identifier {
    import node._
    class Identifier(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : Int =  { 
            symbol_table.getter(_value)
        }
    }
}