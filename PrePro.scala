package prepro

import scala.util.matching.Regex

object PrePro {
    def filter(source_code : String) : String = {
        val pattern = "//.*".r
        val code = pattern.replaceAllIn(source_code, "")

        code.replaceAll("\n", "")
    }

    def prePro(source_code : String) : String = {
        filter(source_code)
    }
}