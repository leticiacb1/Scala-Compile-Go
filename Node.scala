import constants._
import table.SymbolTable

package node {
    abstract class Node (val _value : Any){
        
        var children : List[Node] = Nil

        def add_child(child : Node) : Unit = {
            children = children :+ child
        }

        def evaluate(symbol_table: SymbolTable) : Any

        override def toString () : String = {
            "<Node(value = " + _value +")>"
        }
    }
}

package binop {
    import node._
    class BinOp(_value : Any) extends Node(_value){

        def evaluate(symbol_table: SymbolTable) : Any =  { 
            _value match {

                case Types.PLUS => {
                    children(0).evaluate(symbol_table).asInstanceOf[Int] + children(1).evaluate(symbol_table).asInstanceOf[Int]  
                }

                case Types.MINUS => {
                    children(0).evaluate(symbol_table).asInstanceOf[Int] - children(1).evaluate(symbol_table).asInstanceOf[Int]  
                } 

                case Types.BAR => { 
                    children(0).evaluate(symbol_table).asInstanceOf[Int] / children(1).evaluate(symbol_table).asInstanceOf[Int]
                }

                case Types.TIMES => {
                    children(0).evaluate(symbol_table).asInstanceOf[Int] * children(1).evaluate(symbol_table).asInstanceOf[Int]
                }

                case Types.OR => {
                    children(0).evaluate(symbol_table).asInstanceOf[Boolean] || children(1).evaluate(symbol_table).asInstanceOf[Boolean]
                }

                case Types.AND => {
                    children(0).evaluate(symbol_table).asInstanceOf[Boolean] && children(1).evaluate(symbol_table).asInstanceOf[Boolean]
                }

                case Types.BIGGER_THEN => {
                    children(0).evaluate(symbol_table).asInstanceOf[Int] > children(1).evaluate(symbol_table).asInstanceOf[Int]
                }

                case Types.LESS_THAN => {
                    children(0).evaluate(symbol_table).asInstanceOf[Int] < children(1).evaluate(symbol_table).asInstanceOf[Int]
                }

                case Types.EQUAL_COMP => {
                    children(0).evaluate(symbol_table) == children(1).evaluate(symbol_table)
                }

                case _ => {throw new Exception("Error de tipo")}
            }
        }
    }

}

package intval {
    import node._
    class IntVal(_value : Any) extends Node (_value){
        def evaluate(symbol_table: SymbolTable) : Any =  { 
            _value.asInstanceOf[Int]
        }
    }
}

package unop {
    import node._
    class UnOp(_value : Any) extends Node (_value){
        def evaluate(symbol_table: SymbolTable) : Any =  { 
            _value match {

                case Types.PLUS => {
                    children(0).evaluate(symbol_table).asInstanceOf[Int]
                }

                case Types.MINUS => {
                    -(children(0).evaluate(symbol_table).asInstanceOf[Int])
                }

                case Types.NOT => {
                    !(children(0).evaluate(symbol_table))
                }

                case _  => {throw new Exception("Error de tipo")}
            }
        }
    }
}

package noop {
    import node._
    class NoOp(_value : Any) extends Node (_value){
        def evaluate(symbol_table: SymbolTable) : Unit =  { }
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

package program {
    import node._
    class Program(_value : Any) extends Node (_value){
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
            var result = children(1).evaluate(symbol_table).asInstanceOf[Int]
            symbol_table.setter(children(0)._value.toString , result)
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

    class If(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : Unit =  { 
            var conditional = children(0)
            var block_if    = children(1) 

            if(conditional.evaluate(symbol_table).asInstanceOf[Boolean]) {
                block_if.evaluate(symbol_table)
            }else if(children.size > 2){

                if(!(conditional.evaluate(symbol_table).asInstanceOf[Boolean])){
                    children(2).evaluate(symbol_table)
                }
            }
        }
    }

    class For(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : Unit =  { 
            
            var init_state  = children(0).evaluate(symbol_table)
            var condition   = children(1)
            var increment   = children(2)
            var block       = children(3)

            // Teste para o for, talvez de errado e tenha que usar breakble
            while (condition.evaluate(symbol_table).asInstanceOf[Boolean]) {
                block.evaluate(symbol_table)
                increment.evaluate(symbol_table)
            }
        }
    }

    class Scanln (_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : Int =  { 
            var number = scala.io.StdIn.readLine()
            return number.toInt
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