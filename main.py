from abc import ABC, abstractmethod

import sys 
import re

class Node():

    def __init__(self, element : str , element_before : object , isroot = False):
        
        self.type = None
        self.error = False
        self.is_root = isroot
        
        self.element = element
        self.element_before = element_before

    def evaluate(self):
        if(self.element.isdigit()):
            self.type =  'number'
        else:
            self.type =  'operator'

    def check(self):

        if(not self.is_root):
            if (self.type == self.element_before.type):
                self.error = True
            else:
                self.error = False

        else:
            if(self.is_root and self.type == 'operator'):
                self.error = True 

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
            while(i < len(string_input) and string_input[i].isdigit()):
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

    tree = []
    for idx , var in enumerate(elements):
        
        if(idx == 0):
            if(var.isdigit()):
                
                node = Node(var , '' , True)
                node.evaluate()
                node.check()

                tree.append(node)
            else:
                raise Exception("Sorry, wrong operation")
        
        else:
            
            node = Node(var , tree[idx-1])
            node.evaluate()
            node.check()

            tree.append(node)

        if (node.error):
            raise Exception("Sorry, wrong operation")

    if(tree[-1].type == 'operator'):
        raise Exception("Sorry, wrong operation")

    result = eval(''.join(elements))
    print(f"\n\t > Resultado : {result}")
    return result
    
if __name__ == '__main__':
    std_input = (sys.argv)[1:]
    result = simple_calculator(std_input[0])