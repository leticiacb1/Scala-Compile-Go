package parser

import errors.InvalidExpression
import tokenizer.Tokenizer
import util.control.Breaks._
import constants._
import node._
import binop._
import unop._
import noop._
import intval._
import strval._
import vardec._
import block._
import assigment._
import functions._
import identifier._
import program._

class Parser() {

  def parserAssigment(tokenizer : Tokenizer) : Node = {
    
      var node_identifier = new Identifier(tokenizer.next._value)
      tokenizer.selectNext()

      if(tokenizer.next._type == Types.EQUAL.toString){
        tokenizer.selectNext()

        var bool_expression = parserBoolExpression(tokenizer)

        var node_assigment = new Assigment(Types.EQUAL)
        node_assigment.add_child(node_identifier)
        node_assigment.add_child(bool_expression)
        node_assigment
      
      }else if(tokenizer.next._type == Types.OPEN_PARENTHESES.toString){
        tokenizer.selectNext()

        var funcCall_node = new FuncCall(node_identifier.value)

        while(tokenizer.next._type != Types.CLOSE_PARENTHESES.toString){

          var bool_expression = parserBoolExpression(tokenizer)
          funcCall_node.add_child(bool_expression)

          if(tokenizer.next._type == Types.COMMA.toString){
            tokenizer.selectNext()
          }else{
            break
          }
        }

        if(tokenizer.next._type == Types.CLOSE_PARENTHESES.toString){
          tokenizer.selectNext()
        }else{
          throw new InvalidExpression("\n [PARSER ASSIGMENT] Expected close parentheses token type | Got " + tokenizer.next)
        }
        
        funcCall_node
      }else{
        throw new InvalidExpression("\n [PARSER ASSIGMENT] Expected assigment token | Got " + tokenizer.next)
      }
  }

  def parserFactor(tokenizer : Tokenizer) : Node = {
      if (tokenizer.next._type ==  Types.INT) {
        var node = new IntVal(tokenizer.next._value)
        
        tokenizer.selectNext()
        node
      }

      else if (tokenizer.next._type == Types.VARIABLE_STR){
        var node = new StrVal(tokenizer.next._value)

        tokenizer.selectNext()
        node
      }

      else if (tokenizer.next._type ==  Types.IDENTIFIER){
        var node = new Identifier(tokenizer.next._value)
        tokenizer.selectNext()

        if(tokenizer.next._type == Types.OPEN_PARENTHESES.toString) { 
          var funcCall_node = new FuncCall(node.value)
          tokenizer.selectNext()

          while(tokenizer.next._type != Types.CLOSE_PARENTHESES.toString){
            
            var bool_expression = parserBoolExpression(tokenizer)
            funcCall_node.add_child(bool_expression)

            if(tokenizer.next._type == Types.COMMA.toString){
              tokenizer.selectNext()
            }else{
              break
            }
          }

          if(tokenizer.next._type == Types.CLOSE_PARENTHESES.toString){
            tokenizer.selectNext()
          }else{
            throw new InvalidExpression("\n [FACTOR] Expected close parentheses token type | Got " + tokenizer.next)
          }
          
          node = funcCall_node
        }

        node
      }

      else if (tokenizer.next._type ==  Types.PLUS.toString) {
        
        tokenizer.selectNext()

        var node  = new UnOp(Types.PLUS)
        var child = parserFactor(tokenizer)

        node.add_child(child)
        node
      } 

      else if (tokenizer.next._type ==  Types.MINUS.toString) {
        
        tokenizer.selectNext()

        var node  = new UnOp(Types.MINUS)
        var child = parserFactor(tokenizer)

        node.add_child(child)
        node
      } 

      else if(tokenizer.next._type ==  Types.NOT.toString){
        tokenizer.selectNext()

        var node  = new UnOp(Types.NOT)
        var child = parserFactor(tokenizer)

        node.add_child(child)
        node
      }

      else if (tokenizer.next._type == Types.OPEN_PARENTHESES.toString) {
        
        tokenizer.selectNext()
        var node = parserBoolExpression(tokenizer)

        if(tokenizer.next._type == Types.CLOSE_PARENTHESES.toString){
          
          tokenizer.selectNext()
          node
        }else{
          throw new InvalidExpression("\n [FACTOR] Expected close parentheses type | Got " + tokenizer.next)
        }

      }

      else if(tokenizer.next._type == Types.SCANLN){
        tokenizer.selectNext()

        var node = new Scanln(Types.SCANLN)

        if(tokenizer.next._type == Types.OPEN_PARENTHESES.toString) { 
          tokenizer.selectNext()
          
          if(tokenizer.next._type == Types.CLOSE_PARENTHESES.toString) { 
            tokenizer.selectNext()
            node
          }else{
            throw new InvalidExpression("\n [FACTOR] Expected close parentheses type | Got " + tokenizer.next)
          }


        }else {
          throw new InvalidExpression("\n [FACTOR] Expected open parentheses type | Got " + tokenizer.next)
        }
      }

      else {
        throw new InvalidExpression("\n [FACTOR] Unexpected value in factor | Got " + tokenizer.next)
      }
    }
  
  def parserTerm(tokenizer : Tokenizer) : Node = {
    var operators = List(Types.TIMES.toString , Types.BAR.toString )
    var left_node : Node = parserFactor(tokenizer)

    breakable {
      while(true){
  
        if(! operators.contains(tokenizer.next._type)){
          break;
        }

        else if(tokenizer.next._type == Types.TIMES.toString){
          
          var op_node = new BinOp(Types.TIMES)
          op_node.add_child(left_node)
          
          tokenizer.selectNext()

          var right_node = parserFactor(tokenizer)
          op_node.add_child(right_node)

          left_node = op_node
        }

        else if(tokenizer.next._type == Types.BAR.toString){
          
          var op_node = new BinOp(Types.BAR)
          op_node.add_child(left_node)
          
          tokenizer.selectNext()

          var right_node = parserFactor(tokenizer)
          op_node.add_child(right_node)

          left_node = op_node
        }

        else {
          throw new Exception("\n [TERM] Invalid token : " + tokenizer.next)
        }

      }
    }

    left_node
  }

  def parserExpression(tokenizer : Tokenizer) : Node = {
    var left_node = parserTerm(tokenizer)
    var operators = List(Types.PLUS.toString, Types.MINUS.toString, Types.CONCAT.toString)

    breakable {
      while(true){
  
        if(! operators.contains(tokenizer.next._type)){
          break;
        }

        else if(tokenizer.next._type == Types.MINUS.toString){
          
          var op_node = new BinOp(Types.MINUS)
          op_node.add_child(left_node)
          
          tokenizer.selectNext()

          var right_node = parserTerm(tokenizer)
          op_node.add_child(right_node)

          left_node = op_node
        }

        else if(tokenizer.next._type == Types.PLUS.toString){
          
          var op_node = new BinOp(Types.PLUS)
          op_node.add_child(left_node)
          
          tokenizer.selectNext()

          var right_node = parserTerm(tokenizer)
          op_node.add_child(right_node)

          left_node = op_node
        }

        else if(tokenizer.next._type == Types.CONCAT.toString){
          
          var op_node = new BinOp(Types.CONCAT)
          op_node.add_child(left_node)
          
          tokenizer.selectNext()

          var right_node = parserTerm(tokenizer)
          op_node.add_child(right_node)

          left_node = op_node
        }else {
          throw new Exception("\n [EXPRESSION] Invalid token : " + tokenizer.next)
        }
      }
    }

    left_node
  }

  def parserRlExpression(tokenizer : Tokenizer): Node = {
    var left_node = parserExpression(tokenizer)
    var operators = List(Types.BIGGER_THEN.toString, Types.EQUAL_COMP.toString , Types.LESS_THAN.toString)

    breakable {
      while(true){
  
        if(! operators.contains(tokenizer.next._type)){
          break;
        }

        if(tokenizer.next._type == Types.BIGGER_THEN.toString){
          var op_node = new BinOp(Types.BIGGER_THEN)
          op_node.add_child(left_node)

          tokenizer.selectNext()

          var right_node = parserExpression(tokenizer)
          op_node.add_child(right_node)

          left_node = op_node

        } else if (tokenizer.next._type == Types.LESS_THAN.toString){
          var op_node = new BinOp(Types.LESS_THAN)
          op_node.add_child(left_node)

          tokenizer.selectNext()

          var right_node = parserExpression(tokenizer)
          op_node.add_child(right_node)

          left_node = op_node

        } else if (tokenizer.next._type == Types.EQUAL_COMP){
          var op_node = new BinOp(Types.EQUAL_COMP)
          op_node.add_child(left_node)

          tokenizer.selectNext()

          var right_node = parserExpression(tokenizer)
          op_node.add_child(right_node)

          left_node = op_node
        } else {
          throw new InvalidExpression("\n [RL EXPRESSION] Token type recived | Got " + tokenizer.next)
        }
      }
    }

    left_node
  }

  def parserBoolTerm(tokenizer : Tokenizer): Node = {
    var left_node = parserRlExpression(tokenizer)

    breakable {
      while(true){
  
        if(tokenizer.next._type != Types.AND){
          break;
        }

        if(tokenizer.next._type == Types.AND){
          var op_node = new BinOp(Types.AND)
          op_node.add_child(left_node)

          tokenizer.selectNext()

          var right_node = parserRlExpression(tokenizer)
          op_node.add_child(right_node)

          left_node = op_node
        }else {
          throw new InvalidExpression("\n [BOOL TERM EXPRESSION] Token type recived | Got " + tokenizer.next)
        }
      }
    }

    left_node
  }

  def parserBoolExpression(tokenizer : Tokenizer): Node = {
    var left_node = parserBoolTerm(tokenizer)

    breakable {
      while(true){
  
        if(tokenizer.next._type != Types.OR){
          break;
        }

        if(tokenizer.next._type == Types.OR){
          var op_node = new BinOp(Types.OR)
          op_node.add_child(left_node)

          tokenizer.selectNext()

          var right_node = parserBoolTerm(tokenizer)
          op_node.add_child(right_node)

          left_node = op_node
        }else {
          throw new InvalidExpression("\n [BOOL EXPRESSION] Token type recived | Got " + tokenizer.next)
        }
      }
    }  

    left_node
  }

  def parserStatement(tokenizer : Tokenizer) : Node = {
    var node : Node = null
    
    if(tokenizer.next._type == Types.END_OF_LINE.toString) {
      tokenizer.selectNext()
      node = new NoOp("END_OF_LINE")

    }else if(tokenizer.next._type == Types.IDENTIFIER){
      node = parserAssigment(tokenizer)

    } else if(tokenizer.next._type == Types.PRINTLN) {
      tokenizer.selectNext()

      if(tokenizer.next._type == Types.OPEN_PARENTHESES.toString){
        tokenizer.selectNext()

        var expression = parserBoolExpression(tokenizer)
        
        var node_println = new Println(Types.PRINTLN)
        node_println.add_child(expression)

        if(tokenizer.next._type != Types.CLOSE_PARENTHESES.toString){
          throw new InvalidExpression("\n [STATEMENT] Expected close parentheses token | Got " + tokenizer.next)
        }

        tokenizer.selectNext()
        node = node_println

      }else{
        throw new InvalidExpression("\n [STATEMENT] Expected open parentheses token | Got " + tokenizer.next)
      }

    } else if(tokenizer.next._type == Types.IF) {
      tokenizer.selectNext()

      var bool_expression = parserBoolExpression(tokenizer)
      var block_if        = parserBlock(tokenizer)

      var node_if = new If(Types.IF)
      node_if.add_child(bool_expression)
      node_if.add_child(block_if)

      if(tokenizer.next._type == Types.ELSE){
        tokenizer.selectNext()

        var block_else = parserBlock(tokenizer)
        node_if.add_child(block_else)

      }
      node = node_if

    } else if(tokenizer.next._type == Types.FOR){
      tokenizer.selectNext()

      var init_state = parserAssigment(tokenizer)

      if(tokenizer.next._type == Types.SEMICOLON.toString){
        tokenizer.selectNext()

        var condition = parserBoolExpression(tokenizer)
        
        if(tokenizer.next._type == Types.SEMICOLON.toString){
          tokenizer.selectNext()

          var increment = parserAssigment(tokenizer)
          var block     = parserBlock(tokenizer)

          var node_for  = new For(Types.FOR)
          node_for.add_child(init_state)
          node_for.add_child(condition)
          node_for.add_child(increment)
          node_for.add_child(block)          

          node = node_for

        }else{
          throw new InvalidExpression("\n [STATEMENT] Expected semicolon type | Got " + tokenizer.next)
        }
      }else{
        throw new InvalidExpression("\n [STATEMENT] Expected semicolon type | Got " + tokenizer.next)
      }
    }else if(tokenizer.next._type == Types.VAR){
      tokenizer.selectNext()

      if(tokenizer.next._type == Types.IDENTIFIER){
        var node_identifier = new Identifier(tokenizer.next._value)
        tokenizer.selectNext()

        if((tokenizer.next._type == Types.TYPE_INT) || (tokenizer.next._type == Types.TYPE_STR)){
          var var_dec = new VarDec(tokenizer.next._type)
          var_dec.add_child(node_identifier)

          tokenizer.selectNext()
          if(tokenizer.next._type == Types.EQUAL.toString) {
            tokenizer.selectNext()
            var bool_expression = parserBoolExpression(tokenizer)
            var_dec.add_child(bool_expression)
          }

          node = var_dec
        }else{
          throw new InvalidExpression("\n [STATEMENT] Expected 'type' type | Got " + tokenizer.next)
        }

      }else{
        throw new InvalidExpression("\n [STATEMENT] Expected identifier type | Got " + tokenizer.next)
      }

    }else if(tokenizer.next._type == Types.RETURN){
      tokenizer.selectNext()

      var bool_expression = parserBoolExpression(tokenizer)
      var node_return = new Return("RETURN")
      node_return.add_child(bool_expression)

      node = node_return
    }else{
      throw new InvalidExpression("\n [STATEMENT] Token type recived : " + tokenizer.next)
    }

    // Consumir \n após qualquer uma dessas estruturas
    if(tokenizer.next._type != Types.EOF){
      if(tokenizer.next._type == Types.END_OF_LINE.toString){
        tokenizer.selectNext()
      }else{
        throw new InvalidExpression("\n [STATEMENT] Expected END OF LINE after a statement , got: " + tokenizer.next)
      }
    }

    node
  }  

  def parserBlock(tokenizer : Tokenizer) : Node = {
    var node_block = new Block("BLOCK")
    
    if(tokenizer.next._type == Types.OPEN_KEY.toString) {
      tokenizer.selectNext()
      if(tokenizer.next._type == Types.END_OF_LINE.toString) {
        tokenizer.selectNext()

        breakable {
          while(true){
            if(tokenizer.next._type == Types.CLOSE_KEY.toString){
              break;
            }else{
              var statement = parserStatement(tokenizer)
              node_block.add_child(statement)
            }
          }
        }

        tokenizer.selectNext()
        node_block
      } else {
        throw new InvalidExpression("\n [BLOCK] Expected END OF LINE type | Got " + tokenizer.next)
      }
    }

    node_block
  }

  def declaration(tokenizer : Tokenizer) : Node ={
    var nodeFuncdec = new FuncDec("FUNCDEC")
    var argsList : List[Node] = Nil

    if(tokenizer.next._type != Types.FUNC){
      throw new InvalidExpression("\n [DECLARE] Expected FUNCDEC type | Got " + tokenizer.next)
    }

    tokenizer.selectNext()

    if (tokenizer.next._type != Types.IDENTIFIER) {
      throw new InvalidExpression("\n [DECLARE] Expected IDENTIFIER type | Got " + tokenizer.next)
    }

    var functionName = new Identifier(tokenizer.next._value)
    tokenizer.selectNext()

    if(tokenizer.next._type != Types.OPEN_PARENTHESES.toString){
      throw new InvalidExpression("\n [DECLARE] Expected OPEN PARENTHESES type | Got " + tokenizer.next)
    }

    tokenizer.selectNext()

    while (tokenizer.next._type != Types.CLOSE_PARENTHESES.toString) {
      // Add arguments
      if(tokenizer.next._type == Types.IDENTIFIER){
        var nodeIdentifier = new Identifier(tokenizer.next._value)
        tokenizer.selectNext()

        if(tokenizer.next._type != Types.TYPE_INT || tokenizer.next._type != Types.TYPE_STR){
          throw new InvalidExpression("\n [DECLARE] Invalid type find | Got " + tokenizer.next)
        }

        var arg = new VarDec(tokenizer.next._type)
        arg.add_child(nodeIdentifier)
        argsList = argsList :+ arg
        
        tokenizer.selectNext()

        if(tokenizer.next._type == Types.COMMA.toString) {
          tokenizer.selectNext()
        }else{
          break
        }
      }
    }

    if (tokenizer.next._type != Types.CLOSE_PARENTHESES.toString){
      throw new InvalidExpression("\n [DECLARE] Expected CLOSE PARENTHESES find | Got " + tokenizer.next)
    }
    tokenizer.selectNext()

    if(tokenizer.next._type == Types.TYPE_INT || tokenizer.next._type == Types.TYPE_STR){
      var nodeDefinition = new VarDec(tokenizer.next._type)
      nodeDefinition.add_child(functionName)
      
      tokenizer.selectNext()

      var nodeBlock = parserBlock(tokenizer)

      if(tokenizer.next._type != Types.END_OF_LINE.toString) {
        throw new InvalidExpression("\n [DECLARE] Expected END OF LINE type find | Got " + tokenizer.next)
      }
      tokenizer.selectNext()

      // Add definição da função como filho:
      nodeFuncdec.add_child(nodeDefinition)

      // Add filhos
      for (arg <- argsList) {
          nodeFuncdec.add_child(arg)
      }

      // Add bloco de execuçao da função
      nodeFuncdec.add_child(nodeBlock)

    }else{
      throw new InvalidExpression("\n [DECLARE] Invalid type find | Got " + tokenizer.next)
    }

    return nodeFuncdec
  } 



  def program(tokenizer : Tokenizer) : Node ={
    var node_program = new Program("PROGRAM")

    breakable {
      while(true){
        if(tokenizer.next._type == Types.EOF){
          break;
        }else{
          var statement = parserStatement(tokenizer)
          
          if(tokenizer.next._type == Types.END_OF_LINE.toString){
            tokenizer.selectNext()
          }else{
             throw new InvalidExpression("\n [PROGRAM] Expected END OF LINE type | Got " + tokenizer.next)
          }

          node_program.add_child(statement)
        }
      }
    }

    tokenizer.selectNext()
    node_program

  }

  def run(source_code : String) : Node = {
    
    var tokenizer  = new Tokenizer(source_code)
    tokenizer.selectNext()

    var tree = program(tokenizer)
    
    if(tokenizer.next._type != Types.EOF ){
      throw new InvalidExpression("\n Expected EOF type | Got " + tokenizer.next)
    }

    tree
  }

}