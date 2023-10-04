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

      else if (tokenizer.next._type ==  Types.IDENTIFIER){
        var node = new Identifier(tokenizer.next._value)

        tokenizer.selectNext()
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
        var node = parserExpression(tokenizer)

        if(tokenizer.next._type == Types.CLOSE_PARENTHESES.toString){
          
          tokenizer.selectNext()
          node
        }else{
          throw new InvalidExpression("\n Expected close parentheses type | Got " + tokenizer.next)
        }

      }

      else if(tokenizer.next._type == Types.SCANLN){
        tokenizer.selectNext()

        var node = new Scanln(Types.SCANLN)

        if(tokenizer.next._type == Types.OPEN_PARENTHESES) { 
          tokenizer.selectNext()
          
          if(tokenizer.next._type == Types.CLOSE_PARENTHESES) { 
            tokenizer.selectNext()
            node
          }else{
            throw new InvalidExpression("\n Expected close parentheses type | Got " + tokenizer.next)
          }


        }else {
          throw new InvalidExpression("\n Expected open parentheses type | Got " + tokenizer.next)
        }
      }

      else {
        throw new InvalidExpression("\n Unexpected value in factor | Got " + tokenizer.next)
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
          throw new Exception("Token inválido")
        }

      }
    }

    left_node
  }

  def parserExpression(tokenizer : Tokenizer) : Node = {
    var left_node = parserTerm(tokenizer)
    var operators = List(Types.PLUS.toString, Types.MINUS.toString)

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
        else {
          throw new Exception("Token inválido")
        }
      }
    }

    left_node
  }

  def parserRlExpression(tokenizer : Tokenizer): Node = {
    
    var left_node = parserExpression(tokenizer)
    var operators = List(Types.BIGGER_THEN.toString, Types.EQUAL_COMP.toString , Types.LESS_THAN)

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

        if(tokenizer.next._type != Types.AND){
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

        if(tokenizer.next._type != Types.OR){
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
    
    if(tokenizer.next._type == Types.END_OF_LINE.toString) {
      tokenizer.selectNext()
      new NoOp("END_OF_LINE")

    }else if(tokenizer.next._type == Types.IDENTIFIER){
      parserAssigment(tokenizer)

    } else if(tokenizer.next._type == Types.PRINTLN) {
      tokenizer.selectNext()

      if(tokenizer.next._type == Types.OPEN_PARENTHESES.toString){
        tokenizer.selectNext()

        var expression = parserExpression(tokenizer)

        var node_println = new Println(Values.PRINTLN)
        node_println.add_child(expression)

        if(tokenizer.next._type != Types.CLOSE_PARENTHESES.toString){
          throw new InvalidExpression("\n [STATEMENT] Expected close parentheses token | Got " + tokenizer.next)
        }

        tokenizer.selectNext()
        node_println

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
      node_if

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

          node_for

        }else{
          throw new InvalidExpression("\n [STATEMENT] Expected semicolon type | Got " + tokenizer.next)
        }
      }else{
        throw new InvalidExpression("\n [STATEMENT] Expected semicolon type | Got " + tokenizer.next)
      }

    }else{
      throw new InvalidExpression("\n [STATEMENT] Token type recived : " + tokenizer.next)
    }

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
        throw new InvalidExpression("\n Expected END OF LINE type | Got " + tokenizer.next)
      }
    }

    node_block
  }

  def program(tokenizer : Tokenizer) : Node ={
    var node_program = new Program("PROGRAM")

    breakable {
      while(true){
        if(tokenizer.next._type == Types.EOF){
          break;
        }else{
          var statement = parserStatement(tokenizer)
          node_program.add_child(statement)
        }
      }
    }

    // TEM ?
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