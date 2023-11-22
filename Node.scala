import constants._
import scala.util.control.Breaks._
import table.SymbolTable
import table.FunctionTable
import constants._
import errors._

package node {
    abstract class Node (val _value : Any){
        
        var children : List[Node] = Nil

        def add_child(child : Node) : Unit = {
            children = children :+ child
        }

        def evaluate(symbol_table: SymbolTable) : (Any , Any)

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
                        throw new IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in + operation : {$type1} + {$type2}")
                    }

                    ((value1.asInstanceOf[Int] + value2.asInstanceOf[Int]) , Types.TYPE_INT)  
                }

                case Types.MINUS => {
                    if( !((type1 == type2) && (type1 == Types.TYPE_INT)) ){
                        throw new IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in - operation : {$type1} - {$type2}")
                    }

                    ((value1.asInstanceOf[Int] - value2.asInstanceOf[Int]) , Types.TYPE_INT)
                } 

                case Types.BAR => { 
                    if( !((type1 == type2) && (type1 == Types.TYPE_INT)) ){
                        throw new IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in / operation : {$type1} / {$type2}")
                    }

                    (value1.asInstanceOf[Int]/value2.asInstanceOf[Int] , Types.TYPE_INT)
                }

                case Types.TIMES => {
                    if( !((type1 == type2) && (type1 == Types.TYPE_INT)) ){
                        throw new IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in * operation : {$type1} * {$type2}")
                    }

                   (value1.asInstanceOf[Int]*value2.asInstanceOf[Int] , Types.TYPE_INT)
                }

                case Types.OR => {
                    if( !((type1 == type2) && (type1 == Types.TYPE_INT)) ){
                        throw new IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in OR operation : {$type1} || {$type2}")
                    }

                    //(if (value1.asInstanceOf[Boolean] || value2.asInstanceOf[Boolean]) 1 else 0 ,   Types.TYPE_INT)
                    var boolValue1 = value1.asInstanceOf[Int] != 0
                    var boolValue2 = value2.asInstanceOf[Int] != 0

                    (if (boolValue1 || boolValue2) 1 else 0 ,   Types.TYPE_INT)
                }

                case Types.AND => {
                    if( !((type1 == type2) && (type1 == Types.TYPE_INT)) ){
                        throw new IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in AND operation : {$type1} && {$type2}")
                    }

                    var boolValue1 = value1.asInstanceOf[Int] != 0
                    var boolValue2 = value2.asInstanceOf[Int] != 0

                    //(if (value1.asInstanceOf[Boolean] && value2.asInstanceOf[Boolean]) 1 else 0 ,   Types.TYPE_INT)
                    (if (boolValue1 && boolValue2) 1 else 0 ,   Types.TYPE_INT)
                }

                case Types.BIGGER_THEN => {
                    if(type1 != type2){
                        throw new IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in > operation : {$type1} > {$type2}")
                    }
                    
                    if(type1 == Types.TYPE_INT){
                        (if (value1.asInstanceOf[Int] > value2.asInstanceOf[Int]) 1 else 0 ,   Types.TYPE_INT)        
                    }else{
                        (if (value1.asInstanceOf[String] > value2.asInstanceOf[String]) 1 else 0 ,   Types.TYPE_INT)
                    }
                }

                case Types.LESS_THAN => {

                    if(type1 != type2){
                        throw new IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in < operation : {$type1} < {$type2}")
                    }
                    
                    if(type1 == Types.TYPE_INT){
                        (if (value1.asInstanceOf[Int] < value2.asInstanceOf[Int]) 1 else 0 ,   Types.TYPE_INT)
                    }else{
                        (if (value1.asInstanceOf[String] < value2.asInstanceOf[String]) 1 else 0 ,   Types.TYPE_INT)
                    }
                }

                case Types.EQUAL_COMP => {
                    if(type1 != type2){
                        throw new IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in == operation : {$type1} == {$type2}")
                    }

                    (if (value1 == value2) 1 else 0 ,   Types.TYPE_INT)
                }

                case Types.CONCAT => {
                    (value1.toString + value2.toString  ,  Types.TYPE_STR)
                }

                case _ => {throw new InvalidOperators(s" [BinOp - Evaluate] Invalid operator type = ${_value}")}
            }
        }
    }

}

package intval {
    import node._
    class IntVal(_value : Any) extends Node (_value){
        def evaluate(symbol_table: SymbolTable) : (Any, String) =  { 
            ( _value.asInstanceOf[Int] , Types.TYPE_INT )
        }
    }
}

package strval {
    import node._
    class StrVal(_value : Any) extends Node (_value){
        def evaluate(symbol_table: SymbolTable) : (Any, String) =  { 
            ( _value.asInstanceOf[String] , Types.TYPE_STR )
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
                    ((1)*value.asInstanceOf[Int] , _type.asInstanceOf[String])
                }

                case Types.MINUS => {
                    ((-1)*value.asInstanceOf[Int] , _type.asInstanceOf[String])
                }

                case Types.NOT => {
                    var boolValue = value.asInstanceOf[Int] != 0
                    (if (boolValue) 0 else 1 ,   Types.TYPE_INT)
                }

                case _  => {throw new InvalidOperators(s" [UnOp - Evaluate] Invalid operator type = ${_value}")}
            }
        }
    }
}

package noop {
    import node._
    class NoOp(_value : Any) extends Node (_value){
        def evaluate(symbol_table: SymbolTable) : (Unit, Unit) =  { (Unit , Unit) }
    }
}

package block {
    import node._
    class Block(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : (Any, Any) =  { 
            var result : (Any, Any) = (Unit, Unit)
            var i = 0

            breakable{
                while(i < children.length) {
                    val child = children(i)
                    println(s" Filho : ${child}\n")
                    if(child._value != "END_OF_LINE") {
                        result = child.evaluate(symbol_table)
                    } else {
                        child.evaluate(symbol_table)
                    }

                    if(child._value == "RETURN"){
                        println(" > Achei o retorno \n")
                        break
                    }

                    i +=1
                }
            }
            
            result
        }
    }
}

package program {
    import node._
    class Program(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : (Unit, Unit) =  { 
            for (child <- children) {
                child.evaluate(symbol_table)
            }
            (Unit , Unit)
        }
    }
}


package assigment {
    import node._
    class Assigment(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : (Unit, Unit) =  { 
            var (old_identifier_value , type1) = symbol_table.getter(children(0)._value.asInstanceOf[String])
            var (result_expression, type2) = children(1).evaluate(symbol_table)
            
            if(type1 != type2) {
                throw new IncompatibleTypes(s" [ASSIGMENT - EVALUATE] Setting a value type [$type2] inconsistent with the variable type [$type1]")
            }

            if(type1 == Types.TYPE_INT){
                symbol_table.setter(children(0)._value.asInstanceOf[String] , result_expression.asInstanceOf[Int])
            }else{
                symbol_table.setter(children(0)._value.asInstanceOf[String] , result_expression.asInstanceOf[String])
            }

            (Unit , Unit)
        }
    }
}

package functions {
    import node._
    class Println(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : (Unit , Unit) =  { 
            var (expression_result , _type) = children(0).evaluate(symbol_table)
            println(expression_result)
            (Unit , Unit)
        }
    }

    class If(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : (Unit , Unit) =  { 

            var conditional = children(0)
            var block_if    = children(1) 
            
            var (value , _type) = conditional.evaluate(symbol_table)
            var boolValue = value != 0
            
            if(boolValue) {
                block_if.evaluate(symbol_table)
            }else if(children.size > 2){
                if(!(boolValue)){
                    children(2).evaluate(symbol_table)
                }
            }

            (Unit , Unit)
        }
    }

    class For(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : (Unit , Unit) =  { 
            children(0).evaluate(symbol_table)
            var condition   = children(1)
            var increment   = children(2)
            var block       = children(3)

            // Teste para o for, talvez de errado e tenha que usar breakble
            var (value, _type) = condition.evaluate(symbol_table)
            var boolValue = value != 0

            while (boolValue) {
                block.evaluate(symbol_table)
                increment.evaluate(symbol_table)

                var result = condition.evaluate(symbol_table)
                value = result._1

                boolValue = value != 0
            }

            (Unit , Unit)
        }
    }

    class Scanln (_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : (Int , String) =  { 
            var number = scala.io.StdIn.readLine()

            if(!number.forall(_.isDigit)){
                throw new InvalidType(f" [SCANLN - EVALUATE] Only the integer type is accepted in the Scanln function. Tried type: ${number.getClass.getSimpleName}")
            }

            (number.toInt , Types.TYPE_INT)
        }
    }

}

package identifier {
    import node._
    class Identifier(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : (Any , String) =  { 
            var (value , _type) = symbol_table.getter(_value.toString)

            if(_type == Types.TYPE_STR){
                (value.asInstanceOf[String], _type.asInstanceOf[String])
            }else{
                (value.asInstanceOf[Int], _type.asInstanceOf[String])
            }
            
        }
    }
}

package vardec {
    import node._
    class VarDec(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : (Unit , Unit) =  {
            var type1 = _value
            symbol_table.create(children(0)._value.asInstanceOf[String] , type1.asInstanceOf[String])
            
            if(children.size == 2){
                var (boolExpression , type2) = children(1).evaluate(symbol_table)
                
                if(type1 == type2){
                    if(type2 == Types.TYPE_INT){
                        symbol_table.setter(children(0)._value.asInstanceOf[String] , boolExpression.asInstanceOf[Int])
                    }else{
                        symbol_table.setter(children(0)._value.asInstanceOf[String] , boolExpression.asInstanceOf[String])
                    }
                }else{
                    throw new IncompatibleTypes(s" [VARDEC - EVALUATE] Setting a value type [$type2] inconsistent with the variable type [$type1]")
                }
            }
            
            (Unit, Unit)
        }
    }
}

package func {
    import node._

    class FuncDec(_value : Any) extends Node (_value){

        var function_table = new FunctionTable()

        def evaluate(symbol_table : SymbolTable) : (Unit , Unit) =  { 
            
            var node_declaration = children(0)
            var function_name = node_declaration.children(0)._value.asInstanceOf[String]

            function_table.declare(function_name, this, node_declaration._value.asInstanceOf[String])
            //function_table.show_table()
            (Unit, Unit)
        }
    }

    class Return(_value : Any) extends Node (_value){

        def evaluate(symbol_table : SymbolTable) : (Any , String) =  { 
            
            var bool_expression = children(0)
            var (value, _type) = bool_expression.evaluate(symbol_table)

            if(_type == Types.TYPE_STR){
                (value.asInstanceOf[String], _type.asInstanceOf[String])
            }else{
                (value.asInstanceOf[Int], _type.asInstanceOf[String])
            }
        }
    }

    class FuncCall(_value : Any) extends Node (_value){
        var function_table = new FunctionTable()

        def evaluate(symbol_table : SymbolTable) : (Any , Any) =  { 

            //function_table.show_table()
            var local_table = new SymbolTable()

            // Argumentos passados para a função
            var function_name = _value.asInstanceOf[String]
            var received_args = children
            // Consultando a declaracao da função e o tipo dos argumentos esperados
            function_table.show_table()

            var (function_node, return_expected_type) = function_table.getter(function_name) 

            var declaration = function_node.children.head
            var expected_args =  function_node.children.slice(1, function_node.children.length - 1)
            var block = function_node.children.last
            
            //  Mesma quantidade de argumentos passados e requeridos pela função:
            if(received_args.length != expected_args.length){
                throw new InvalidExpression("\n [FUNCALL- EVALUATE] Incorrect number of arguments: expected " + expected_args.length + " got " + received_args.length )
            }

            // Varrendo argumentos passados e argumentos esperados
            for (i <- received_args.indices) {
                expected_args(i).evaluate(local_table)
                
                var expected_type = expected_args(i)._value
                
                var identifier = expected_args(i).children(0)
                
                var (received_value, received_type) = received_args(i).evaluate(symbol_table)
                if(received_type != expected_type){
                    throw new InvalidExpression("\n [FUNCALL- EVALUATE] Incorrect arg type for " + identifier._value + " in " + function_name +  " function. Expected type " +  expected_type +  " got " + received_type)
                }

                // Setando o valor recebido do argumento
                local_table.setter(identifier._value.asInstanceOf[String], received_value)
            }

            // Executando o conteúdo da função
            var (received_return_value , received_return_type) = block.evaluate(local_table)

            if((!received_return_value.isInstanceOf[Unit]) && (!received_return_type.isInstanceOf[Unit])){
                if(received_return_type != return_expected_type){
                     throw new InvalidExpression("\n [FUNCALL- EVALUATE] Incorrect return type in " + function_name + " function. Expected type" + return_expected_type + " got " + received_return_type)
                }
            }

            (received_return_value , received_return_type)
        }
    }
}