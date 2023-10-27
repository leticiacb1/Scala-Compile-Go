import constants._
import table.SymbolTable
import constants._
import errors._

package node {
    abstract class Node (val _value : Any){
        
        var children : List[Node] = Nil

        def add_child(child : Node) : Unit = {
            children = children :+ child
        }

        def evaluate(symbol_table: SymbolTable) : (Any , String)

        override def toString () : String = {
            "<Node(value = " + _value +")>"
        }
    }
}

package binop {
    import node._
    class BinOp(_value : Any) extends Node(_value){

        def evaluate(symbol_table: SymbolTable) : (Any , String) =  { 
            
            var (value1 , type1) = children(0).evaluate(symbol_table)
            var (value2 , type2) = children(1).evaluate(symbol_table)
            
            _value match {

                case Types.PLUS => {
                    if( !((type1 == type2) && (type1 == Types.TYPE_INT)) ){
                        IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in + operation : {$type1} + {$type2}")
                    }

                    (value1 + value2 , Types.TYPE_INT)  
                }

                case Types.MINUS => {
                    if( !((type1 == type2) && (type1 == Types.TYPE_INT)) ){
                        IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in - operation : {$type1} - {$type2}")
                    }

                    (value1 - value2 , Types.TYPE_INT)
                } 

                case Types.BAR => { 
                    if( !((type1 == type2) && (type1 == Types.TYPE_INT)) ){
                        IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in / operation : {$type1} / {$type2}")
                    }

                    (value1/value2 , Types.TYPE_INT)
                }

                case Types.TIMES => {
                    if( !((type1 == type2) && (type1 == Types.TYPE_INT)) ){
                        IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in * operation : {$type1} * {$type2}")
                    }

                   (value1*value2 , Types.TYPE_INT)
                }

                case Types.OR => {

                    if( !((type1 == type2) && (type1 == Types.TYPE_INT)) ){
                        IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in OR operation : {$type1} || {$type2}")
                    }

                    (if (value1 || value2) 1 else 0 ,   Types.TYPE_INT)
                }

                case Types.AND => {
                    if( !((type1 == type2) && (type1 == Types.TYPE_INT)) ){
                        IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in AND operation : {$type1} && {$type2}")
                    }

                    (if (value1 && value2) 1 else 0 ,   Types.TYPE_INT)
                }

                case Types.BIGGER_THEN => {

                    if(type1 != type2){
                        IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in > operation : {$type1} > {$type2}")
                    }

                    (if (value1 > value2) 1 else 0 ,   Types.TYPE_INT)
                }

                case Types.LESS_THAN => {

                    if(type1 != type2){
                        IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in < operation : {$type1} < {$type2}")
                    }

                    (if (value1 < value2) 1 else 0 ,   Types.TYPE_INT)
                }

                case Types.EQUAL_COMP => {
                    if(type1 != type2){
                        IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in == operation : {$type1} == {$type2}")
                    }

                    (if (value1 == value2) 1 else 0 ,   Types.TYPE_INT)
                }

                case Types.CONCAT => {
                    (value1x.toString + value2x.toString  ,  Types.TYPE_STR)
                }

                case _ => {throw new InvalidOperators(s" [BinOp - Evaluate] Invalid operator type = $_value")}
            }
        }
    }

}

package intval {
    import node._
    class IntVal(_value : Any) extends Node (_value){
        def evaluate(symbol_table: SymbolTable) : (Any, String) =  { 
            ( _value.asInstanceOf[Int] , types.TYPE_INT )
        }
    }
}

package unop {
    import node._
    class UnOp(_value : Any) extends Node (_value){
        def evaluate(symbol_table: SymbolTable) : (Any, String) =  { 
            
            var (value , _type) = children(0).evaluate(symbol_table)
            
            _value match {

                case Types.PLUS => {
                    ((1)*value , _type)
                }

                case Types.MINUS => {
                    ((-1)*value , _type)
                }

                case Types.NOT => {
                    (!value , _type)
                }

                case _  => {throw new InvalidOperators(s" [UnOp - Evaluate] Invalid operator type = $_value")}
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

            var (identifier , type1) = symbol_table.getter(children(0)._value)
            var (result_expression, type2) = children(1).evaluate(symbol_table)

            if(type1 != type2) {
                IncompatibleTypes(s" [ASSIGMENT - EVALUATE] Setting a value type [$type2] inconsistent with the variable type [$type1]")
            }

            symbol_table.setter(children(0)._value , reresult_expressionsult)
        }
    }
}

package functions {
    import node._
    class Println(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : Unit =  { 
            var (expression_result , _type) = children(0).evaluate(symbol_table)
            println(expression_result)
        }
    }

    class If(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : Unit =  { 
            var conditional = children(0)
            var block_if    = children(1) 

            var value , _type = conditional.evaluate(symbol_table)
            if(value) {
                block_if.evaluate(symbol_table)
            }else if(children.size > 2){
                if(!(value)){
                    children(2).evaluate(symbol_table)
                }
            }
        }
    }

    class For(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : Unit =  { 
            
            children(0).evaluate(symbol_table)
            var condition   = children(1)
            var increment   = children(2)
            var block       = children(3)

            // Teste para o for, talvez de errado e tenha que usar breakble
            var (value, _type) = condition.evaluate(symbol_table)
            while (value) {
                block.evaluate(symbol_table)
                increment.evaluate(symbol_table)

                (value, _type) = condition.evaluate(symbol_table)
            }
        }
    }

    class Scanln (_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : (Any , String) =  { 
            var number = scala.io.StdIn.readLine()

            if(!number.isDigit){
                var _type = number.getClass.getSimpleName
                throw new InvalidType(f" [SCANLN - EVALUATE] Only the integer type is accepted in the Scanln function. Tried type: $_type")
            }

            (number.toInt , types.TYPE_INT)
        }
    }

}

package identifier {
    import node._
    class Identifier(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : Int =  { 
            var value , _type = symbol_table.getter(_value.toString)
            (value, _type)
        }
    }
}