package parser

import errors.InvalidExpression
import tokenizer.Tokenizer
import util.control.Breaks._
import constants._
import node._
import binop._
import unop._
// import noop._
import intval._

class Parser() {

  def parserFactor(tokenizer : Tokenizer) : Node = {

      if (tokenizer.next._type ==  Types.INT) {
        var node = new IntVal(tokenizer.next._value)
        println(tokenizer.next)
        tokenizer.selectNext()
        node
      }

      else if (tokenizer.next._type ==  Types.PLUS.toString) {
        println(tokenizer.next)
        tokenizer.selectNext()

        var node  = new UnOp(Types.PLUS)
        var child = parserFactor(tokenizer)

        node.add_child(child)
        node
      } 

      else if (tokenizer.next._type ==  Types.MINUS.toString) {
        println(tokenizer.next)
        tokenizer.selectNext()

        var node  = new UnOp(Types.MINUS)
        var child = parserFactor(tokenizer)

        node.add_child(child)
        node
      } 

      else if (tokenizer.next._type == Types.OPEN_PARENTHESES.toString) {
        println(tokenizer.next)
        tokenizer.selectNext()
        var node = parserExpression(tokenizer)

        if(tokenizer.next._type == Types.CLOSE_PARENTHESES.toString){
          println(tokenizer.next)
          tokenizer.selectNext()
          node
        }else{
          throw new InvalidExpression("\n Expected close parentheses type | Got " + tokenizer.next)
        }

      }

      else {
        throw new InvalidExpression("\n Unexpected value in factory | Got " + tokenizer.next)
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
          println(tokenizer.next)
          var op_node = new BinOp(Types.TIMES)
          op_node.add_child(left_node)
          
          tokenizer.selectNext()

          var right_node = parserFactor(tokenizer)
          op_node.add_child(right_node)

          left_node = op_node
        }

        else if(tokenizer.next._type == Types.BAR.toString){
          println(tokenizer.next)
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
          println(tokenizer.next)
          var op_node = new BinOp(Types.MINUS)
          op_node.add_child(left_node)
          
          tokenizer.selectNext()

          var right_node = parserFactor(tokenizer)
          op_node.add_child(right_node)

          left_node = op_node
        }

        else if(tokenizer.next._type == Types.PLUS.toString){
          println(tokenizer.next)
          var op_node = new BinOp(Types.PLUS)
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

  def run(source_code : String) : Node = {
    
    var tokenizer  = new Tokenizer(source_code)
    tokenizer.selectNext()

    var tree = parserExpression(tokenizer)
    //println(tree.children(0).evaluate() , tree.children(1).evaluate())
    
    if(tokenizer.next._type != Types.EOF ){
      throw new InvalidExpression("\n Expected EOF type | Got " + tokenizer.next)
    }

    tree
  }

}