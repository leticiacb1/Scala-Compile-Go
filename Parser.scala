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

class Parser() {

  def parser_assigment(tokenizer : Tokenizer) : Node = {
    
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

  def parserStatement(tokenizer : Tokenizer) : Node = {
    
    if(tokenizer.next._type == Types.END_OF_LINE.toString) {
      tokenizer.selectNext()
      new NoOp("END_OF_LINE")

    }else if(tokenizer.next._type == Types.IDENTIFIER){
      var node_identifier = new Identifier(tokenizer.next._value)
      tokenizer.selectNext()

      if(tokenizer.next._type == Types.EQUAL.toString){
        tokenizer.selectNext()

        var expression = parserExpression(tokenizer)

        var node_assigment = new Assigment(Types.EQUAL)
        node_assigment.add_child(node_identifier)
        node_assigment.add_child(expression)
        node_assigment

      }else{
        throw new InvalidExpression("\n [STATEMENT] Expected assigment token | Got " + tokenizer.next)
      }

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

    }else{
      throw new InvalidExpression("\n [STATEMENT] Token type recived : " + tokenizer.next)
    }

  }  

  def parserBlock(tokenizer : Tokenizer) : Node = {
    var node_block = new Block("BLOCK")
    
    breakable {
      while(true){
        if(tokenizer.next._type == Types.EOF){
          break;
        }else{
          var statement = parserStatement(tokenizer)
          node_block.add_child(statement)
        }
      }
    }

    node_block   
  }

  def run(source_code : String) : Node = {
    
    var tokenizer  = new Tokenizer(source_code)
    tokenizer.selectNext()

    var tree = parserBlock(tokenizer)
    
    if(tokenizer.next._type != Types.EOF ){
      throw new InvalidExpression("\n Expected EOF type | Got " + tokenizer.next)
    }

    tree
  }

}