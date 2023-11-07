import constants._
import table.SymbolTable
import constants._
import errors._
import assembler.Assembler
import id.Counter

package node {
    abstract class Node (val _value : Any){
        
        var children : List[Node] = Nil
        
        var idCounter = new Counter()
        var id  = idCounter.getCounter
        idCounter.incrementCounter()

        var asm = new Assembler()

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
            
            var (value2 , type2) = children(1).evaluate(symbol_table)
            asm.appendToBody(s"""PUSH EAX\n\n""")
            var (value1 , type1) = children(0).evaluate(symbol_table)
            asm.appendToBody(s"""POP EBX\n\n""")

            _value match {

                case Types.PLUS => {
                    if( !((type1 == type2) && (type1 == Types.TYPE_INT)) ){
                        throw new IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in + operation : {$type1} + {$type2}")
                    }
                    
                    asm.appendToBody(s"""; Binop(${value1} + ${value2})\n""")
                    asm.appendToBody(s"""ADD EAX , EBX\n\n""")
                    
                    ((value1.asInstanceOf[Int] + value2.asInstanceOf[Int]) , Types.TYPE_INT)  
                }

                case Types.MINUS => {
                    if( !((type1 == type2) && (type1 == Types.TYPE_INT)) ){
                        throw new IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in - operation : {$type1} - {$type2}")
                    }

                    asm.appendToBody(s"""; Binop(${value1} - ${value2})\n""")
                    asm.appendToBody(s"""SUB EAX , EBX\n\n""")

                    ((value1.asInstanceOf[Int] - value2.asInstanceOf[Int]) , Types.TYPE_INT)
                } 

                case Types.BAR => { 
                    if( !((type1 == type2) && (type1 == Types.TYPE_INT)) ){
                        throw new IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in / operation : {$type1} / {$type2}")
                    }

                    asm.appendToBody(s"""; Binop(${value1} / ${value2})\n""")
                    asm.appendToBody(s"""IDIV EBX\n\n""")

                    (value1.asInstanceOf[Int]/value2.asInstanceOf[Int] , Types.TYPE_INT)
                }

                case Types.TIMES => {
                    if( !((type1 == type2) && (type1 == Types.TYPE_INT)) ){
                        throw new IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in * operation : {$type1} * {$type2}")
                    }

                    asm.appendToBody(s"""; Binop(${value1} * ${value2})\n""")
                    asm.appendToBody(s"""IMUL EAX , EBX\n\n""")

                    (value1.asInstanceOf[Int]*value2.asInstanceOf[Int] , Types.TYPE_INT)
                }

                case Types.OR => {
                    if( !((type1 == type2) && (type1 == Types.TYPE_INT)) ){
                        throw new IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in OR operation : {$type1} || {$type2}")
                    }

                    var boolValue1 = value1.asInstanceOf[Int] != 0
                    var boolValue2 = value2.asInstanceOf[Int] != 0
                 
                    asm.appendToBody(s"""; Binop(${value1} || ${value2})\n""")
                    asm.appendToBody(s"""OR EAX , EBX\n\n""")

                    (if (boolValue1 || boolValue2) 1 else 0 ,   Types.TYPE_INT)
                }

                case Types.AND => {
                    if( !((type1 == type2) && (type1 == Types.TYPE_INT)) ){
                        throw new IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in AND operation : {$type1} && {$type2}")
                    }

                    var boolValue1 = value1.asInstanceOf[Int] != 0
                    var boolValue2 = value2.asInstanceOf[Int] != 0

                    asm.appendToBody(s"""; Binop(${value1} && ${value2})\n""")
                    asm.appendToBody(s"""AND EAX , EBX\n\n""")

                    (if (boolValue1 && boolValue2) 1 else 0 ,   Types.TYPE_INT)
                }

                case Types.BIGGER_THEN => {
                    if(type1 != type2){
                        throw new IncompatibleTypes(s" [Binop - Evaluate] Incompatible Types find in > operation : {$type1} > {$type2}")
                    }

                    asm.appendToBody(s"""; Binop(${value1} > ${value2})\n""")
                    asm.appendToBody(s"""CMP EAX, EBX \n""")
                    asm.appendToBody(s"""CALL binop_jg  \n\n""")
                    
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

                    asm.appendToBody(s"""; Binop(${value1} < ${value2})\n""")
                    asm.appendToBody(s"""CMP EAX, EBX \n""")
                    asm.appendToBody(s"""CALL binop_jl  \n\n""")
                    
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

                    asm.appendToBody(s"""; Binop(${value1} == ${value2})\n""")
                    asm.appendToBody(s"""CMP EAX, EBX \n""")
                    asm.appendToBody(s"""CALL binop_je  \n\n""")

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

            asm.appendToBody(s"""; Intval(value = ${_value})\n""")
            asm.appendToBody(s"""MOV EAX , ${_value} \n\n""")
        
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

                    asm.appendToBody(s"""; UnOp(value = ${_value})\n""")
                    asm.appendToBody(s"""MOV EAX , ${_value} \n\n""")

                    ((1)*value.asInstanceOf[Int] , _type.asInstanceOf[String])
                }

                case Types.MINUS => {

                    asm.appendToBody(s"""; UnOp(value = ${_value})\n""")
                    asm.appendToBody(s"""NEG EAX  \n\n""")

                    ((-1)*value.asInstanceOf[Int] , _type.asInstanceOf[String])
                }

                case Types.NOT => {

                    var boolValue = value.asInstanceOf[Int] != 0
                    
                    asm.appendToBody(s"""; UnOp(value = ${_value})\n""")
                    asm.appendToBody(s"""MOV EAX  , ${if (boolValue) 0 else 1} \n\n""")

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
        def evaluate(symbol_table : SymbolTable) : (Unit, Unit) =  { 
            for (child <- children) {
                child.evaluate(symbol_table)
            }
            (Unit , Unit)
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
            var position = symbol_table.get_position(children(0)._value.asInstanceOf[String])
            var (result_expression, type2) = children(1).evaluate(symbol_table)
            
            if(type1 != type2) {
                throw new IncompatibleTypes(s" [ASSIGMENT - EVALUATE] Setting a value type [$type2] inconsistent with the variable type [$type1]")
            }

            if(type1 == Types.TYPE_INT){
 
                asm.appendToBody(s"""; Assigment(identifier = ${children(0)._value} , value = ${result_expression})\n""")
                asm.appendToBody(s"""MOV[EBP - ${position}], EAX \n\n""")

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
            
            asm.appendToBody(s"""; Println\n""")
            asm.appendToBody(s"""PUSH EAX\n""")
            asm.appendToBody(s"""PUSH formatout\n""")
            asm.appendToBody(s"""CALL printf\n""")
            asm.appendToBody(s"""ADD ESP , 8\n\n""")

            println(expression_result)
            (Unit , Unit)
        }
    }

    class If(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : (Unit , Unit) =  { 
            
            var instruction : String = ""
            
            var conditional = children(0)
            var block_if    = children(1) 
            
            var (value , _type) = conditional.evaluate(symbol_table)
            
            asm.appendToBody(s"""IF_${id}:\n""")
            asm.appendToBody(s"""CMP EAX , False \n""")
            asm.appendToBody(s"""JMP ELSE_${id}\n\n""")
            
            //Bloco If
            block_if.evaluate(symbol_table)
            asm.appendToBody(s"""JMP EXIT_IF_${id}\n\n""")

            // Bloco Else
            asm.appendToBody(s"""ELSE_${id}:\n\n""")

            if(children.size > 2){
                children(2).evaluate(symbol_table)
            }

            //Fim
            asm.appendToBody(s"""EXIT_IF_${id}: \n\n""")

            (Unit , Unit)
        }
    }

    class For(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : (Unit , Unit) =  { 
            
            var init_state  = children(0)
            var condition   = children(1)
            var increment   = children(2)
            var block       = children(3)
            
            // Inicialização
            init_state.evaluate(symbol_table) 
            asm.appendToBody(s"""LOOP_${id}:\n""")
            
            // Condição
            condition.evaluate(symbol_table)
            asm.appendToBody(s"""CMP EAX , False\n""")
            asm.appendToBody(s"""JE EXIT_LOOP_${id}\n""")

            //Bloco de Instruções
            block.evaluate(symbol_table)

            //Incremento
            increment.evaluate(symbol_table)
            asm.appendToBody(s"""JMP LOOP_${id}\n\n""")
            asm.appendToBody(s"""EXIT_LOOP_${id}:\n\n""")

            (Unit , Unit)
        }
    }

    class Scanln (_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : (Int , String) =  { 
            var number = scala.io.StdIn.readLine()

            if(!number.forall(_.isDigit)){
                throw new InvalidType(f" [SCANLN - EVALUATE] Only the integer type is accepted in the Scanln function. Tried type: ${number.getClass.getSimpleName}")
            }
            
            asm.appendToBody(s"""; Scanln\n""")
            asm.appendToBody(s"""PUSH scanint\n""")
            asm.appendToBody(s"""PUSH formatin\n""")
            asm.appendToBody(s"""CALL scanf\n""")
            asm.appendToBody(s"""ADD ESP , 8""")
            asm.appendToBody(s"""MOV EAX , DWORD [scanint]\n""")
            asm.appendToBody(s"""MOV [EBP - 4] , EAX \n\n""")

            (number.toInt , Types.TYPE_INT)
        }
    }

}

package identifier {
    import node._
    class Identifier(_value : Any) extends Node (_value){
        def evaluate(symbol_table : SymbolTable) : (Any , String) =  { 
            var (value , _type) = symbol_table.getter(_value.toString)
            var position = symbol_table.get_position(_value.toString)
            
            asm.appendToBody(s"""; Identifier(value = ${_value})\n""")
            asm.appendToBody(s"""MOV EAX , [EBP - ${position}]\n\n""")

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
            
            asm.appendToBody(s"""; Vardec(identifier = ${children(0)._value.asInstanceOf[String]})\n""")
            asm.appendToBody(s"""PUSH DWORD 0 \n\n""")

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