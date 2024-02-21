package prepro

import scala.util.matching.Regex

object PrePro {
    def filter(source_code : String) : String = {
        val pattern = "//.*".r
        val code = pattern.replaceAllIn(source_code, "")
        
        if(code.length() == 1){
            ""
        }else{
            code
        }
    }

    def prePro(source_code : List[String]) : String = {
        
        var code : String = ""
        
        for(line <- source_code){
            code += filter(line)
        }
        code
    }
}