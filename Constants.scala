package constants

object Types { 

    val INVALID           = "INVALID" 
    
    val INT               = "INT"
    
    val IDENTIFIER        = "IDENTIFIER"
    
    val PLUS              = '+'
    val MINUS             = '-'
    val TIMES             = '*'
    val BAR               = '/'
    val EQUAL             = '='
    
    val OPEN_PARENTHESES  = '('
    val CLOSE_PARENTHESES = ')'
    val OPEN_KEY          = '{'
    val CLOSE_KEY         = '}'
    val SEMICOLON         = ';'
    val END_OF_LINE       = '\n'
    val EOF               = "EOF"

    val PRINTLN = "Println"
    val IF      = "if"
    val ELSE    = "else"
    val FOR     = "for"
    val SCANLN  = "Scanln"
    val VAR     = "var"

    val EQUAL_COMP   = "=="
    val BIGGER_THEN  = '>'
    val LESS_THAN    = '<'

    val AND          = "&&"
    val E            = '&'
    val OR           = "||"
    val O            = '|'
    val NOT          = '!'

    val CONCAT       = '.'

    val TYPE_INT     = "int"
    val TYPE_STR     = "string"
    val VARIABLE_STR = "variable_str"

    val QUOTATION_MARKS  = '"'
    val COMMA = ','

    val RETURN = "return"
    val FUNC   = "func"
}

object Values {

    val INVALID     = -1 

    val EOF         = 0
    
    val PLUS        = 1
    val MINUS       = 2
    val TIMES       = 3
    val BAR         = 4
    val EQUAL       = 5
    
    val PARENTHESES = 6
    val KEYS        = 7
    val SEMICOLON   = 8
    val END_OF_LINE = 9

    val PRINTLN     = 10
    val IF          = 11
    val ELSE        = 12
    val FOR         = 13
    val SCANLN      = 14

    val EQUAL_COMP  = 15
    val BIGGER_THEN = 16
    val LESS_THAN   = 17

    val AND         = 18
    val OR          = 19
    val NOT         = 20

    val CONCAT       = 21

    val TYPE_INT     = 22
    val TYPE_STR     = 23
    val VARIABLE_STR = 24

    val QUOTATION_MARKS = 25
    val FUNCTIONS = 26

    val COMMA = 27

    val RETURN = 28
    val FUNC   = 29
    
}