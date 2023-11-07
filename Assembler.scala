package assembler

import java.io._ 

class Assembler() {

    private val header = """
        ; constantes
        SYS_EXIT equ 1
        SYS_READ equ 3
        SYS_WRITE equ 4
        STDIN equ 0
        STDOUT equ 1
        True equ 1
        False equ 0

        segment .data
        formatin: db "%d", 0
        formatout: db "%d", 10, 0 ; newline, null terminator
        scanint: times 4 db 0 ; 32-bit integer = 4 bytes

        segment .bss ; variáveis
        res RESB 1
        extern fflush
        extern stdout

        section .text
        global main ; linux
        extern scanf ; linux
        extern printf ; linux

        ; subrotinas if/while
        binop_je:
            JE binop_true
            JMP binop_false
        binop_jg:
            JG binop_true
            JMP binop_false
        binop_jl:
            JL binop_true
            JMP binop_false
        binop_false:
            MOV EAX, False
            JMP binop_exit
        binop_true:
            MOV EAX, True
        binop_exit:
            RET

        main:
                PUSH EBP ; guarda o base pointer
                MOV EBP, ESP ; estabelece um novo base pointer
        
"""

    private val footer =  """
                PUSH DWORD [stdout]
                CALL fflush
                ADD ESP, 4
                MOV ESP, EBP
                POP EBP
                MOV EAX, 1
                XOR EBX, EBX
                INT 0x80\n
    """

    def appendToBody(newContent: String): Unit = {
        var fomatedContent = formatInstruction(newContent)
        Assembler.appendInstruction(fomatedContent)
    }

    private def formatInstruction (content : String) : String = {
        val withoutTabs = content.replaceAll("\t", "")

        if (content.contains(":")) {
            "\t\t\t" + withoutTabs
        } else {
            "\t\t\t\t" + withoutTabs
        }
        
    }

    private def writeFile(fileName : String , content : String) : Unit = {
        val file = new File(fileName)
        val bw = new BufferedWriter(new FileWriter(file))

        bw.write(content)
        bw.close()
    }

    def mount(fileName : String) : Unit ={
        var parts = fileName.split("\\.")
        var newFileName = parts(0) + ".asm"
        
        var content = header + Assembler.body + footer
        
        try{
            writeFile(newFileName, content)
        } catch {
            case e : Exception => 
                 println("\n [WRITE ASM FILE] Fail to write file | " + e.getMessage)
        }
    }

}


object Assembler {
  private var body = ""

  private def appendInstruction(newContent: String): Unit = {
    body += newContent
  }

  def getBody: String = body
}