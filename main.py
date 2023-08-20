from abc import ABC, abstractmethod

import sys 
import re

def simple_calculator(string_input):
   
    elements = []
    idx = 0
   
    while( idx < len(string_input) ):
        
        char = string_input[idx]
    
        if(char.isdigit()): 
            # Contador interno
            i = idx
            num = ''

            # Loop interno buscando numeros "compostos"
            while(string_input[i].isdigit()):
                num+= string_input[i]
                i+=1
            elements.append(num)

            # Atualiza varredura da string
            idx = i

        else:
            if(char in ['+','-']):
                # Adiciona operadores
                elements.append(char)
            idx+=1
    
    print(f" Lista de elementos : {elements}")

    return 0
    
if __name__ == '__main__':
    std_input = (sys.argv)[1:]
    result = simple_calculator(std_input[0])