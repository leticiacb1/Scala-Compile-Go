
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
        
				; Vardec(identifier = x)
				PUSH DWORD 0 

				; Vardec(identifier = y)
				PUSH DWORD 0 

				; Intval(value = 1)
				MOV EAX , 1 

				PUSH EAX

				; Intval(value = 3)
				MOV EAX , 3 

				POP EBX

				; Binop(3 + 1)
				ADD EAX , EBX

				; Assigment(identifier = x , value = 4)
				MOV[EBP - 4], EAX 

				; Identifier(value = x)
				MOV EAX , [EBP - 4]

				; Assigment(identifier = y , value = 4)
				MOV[EBP - 8], EAX 

				; Intval(value = 1)
				MOV EAX , 1 

				PUSH EAX

				; Identifier(value = x)
				MOV EAX , [EBP - 4]

				POP EBX

				; Binop(4 > 1)
				CMP EAX, EBX 
				CALL binop_jg  

			IF_23:
				CMP EAX , False 
				JMP ELSE_23

				; Intval(value = 1)
				MOV EAX , 1 

				PUSH EAX

				; Intval(value = 5)
				MOV EAX , 5 

				POP EBX

				; Binop(5 - 1)
				SUB EAX , EBX

				; Assigment(identifier = x , value = 4)
				MOV[EBP - 4], EAX 

				JMP EXIT_IF_23

			ELSE_23:

			EXIT_IF_23: 

				; Intval(value = 3)
				MOV EAX , 3 

				PUSH EAX

				; Identifier(value = x)
				MOV EAX , [EBP - 4]

				POP EBX

				; Binop(4 == 3)
				CMP EAX, EBX 
				CALL binop_je  

			IF_28:
				CMP EAX , False 
				JMP ELSE_28

				JMP EXIT_IF_28

			ELSE_28:

				; Intval(value = 3)
				MOV EAX , 3 

				; Assigment(identifier = x , value = 3)
				MOV[EBP - 4], EAX 

			EXIT_IF_28: 

				; Intval(value = 3)
				MOV EAX , 3 

				; Assigment(identifier = x , value = 3)
				MOV[EBP - 4], EAX 

			LOOP_52:
				; Intval(value = 5)
				MOV EAX , 5 

				PUSH EAX

				; Identifier(value = x)
				MOV EAX , [EBP - 4]

				POP EBX

				; Binop(3 < 5)
				CMP EAX, EBX 
				CALL binop_jl  

				CMP EAX , False
				JE EXIT_LOOP_52
				; Intval(value = 1)
				MOV EAX , 1 

				PUSH EAX

				; Identifier(value = x)
				MOV EAX , [EBP - 4]

				POP EBX

				; Binop(3 - 1)
				SUB EAX , EBX

				; Assigment(identifier = y , value = 2)
				MOV[EBP - 8], EAX 

				; Intval(value = 1)
				MOV EAX , 1 

				PUSH EAX

				; Identifier(value = x)
				MOV EAX , [EBP - 4]

				POP EBX

				; Binop(3 + 1)
				ADD EAX , EBX

				; Assigment(identifier = x , value = 4)
				MOV[EBP - 4], EAX 

				JMP LOOP_52

			EXIT_LOOP_52:

				; Identifier(value = x)
				MOV EAX , [EBP - 4]

				; Println
				PUSH EAX
				PUSH formatout
				CALL printf
				ADD ESP , 8


                PUSH DWORD [stdout]
                CALL fflush
                ADD ESP, 4
                MOV ESP, EBP
                POP EBP
                MOV EAX, 1
                XOR EBX, EBX
                INT 0x80
    