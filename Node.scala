import constants._

package node {
    abstract class Node (val _value : Any){
        
        var children : List[Node] = Nil

        def add_child(child : Node) : Unit = {
            children = children :+ child

            if (children.size > 2) {
                throw new Exception(" Muitos filhos")
            }

        }

        def evaluate() : Int

    }
}

package binop {
    import node._
    class BinOp(_value : Any) extends Node(_value){

        def evaluate() : Int =  { 
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
        def evaluate() : Int =  { 
            _value.asInstanceOf[Int]
        }
    }
}

package unop {
    import node._
    class UnOp(_value : Any) extends Node (_value){
        def evaluate() : Int =  { 
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
    class NoOp(var _value : Any){
        def evaluate() : Unit =  { 
            None
        }
    }
}