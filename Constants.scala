package constants

object Types { 
    val INVALID           = "INVALID" 
    val EOF               = "EOF" 
    val INT               = "INT"
    val IDENTIFIER        = "IDENTIFIER"
    val PLUS              = '+'
    val MINUS             = '-'
    val TIMES             = '*'
    val BAR               = '/'
    val EQUAL             = '='
    val OPEN_PARENTHESES  = '('
    val CLOSE_PARENTHESES = ')'
    val END_OF_LINE       = '\n'
    val PRINTLN           = "print"
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
    val END_OF_LINE = 7
    val PRINTLN     = 8
}