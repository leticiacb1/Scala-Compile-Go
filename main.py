import sys 
import re

class Node():
    ...

def simple_calculator(operation):

    without_spaces = ''.join(operation)

    print(without_spaces) 
    
    for element in operation:
        
        if(element.isdigit()):
            print(f'Numero : {element}')
        else:
            
            if(element in ['+','-']):
                print(f'Operador : {element}')
            else:
                print(f'É um conjunto: {element}')

    return 0
    
if __name__ == '__main__':
    std_input = (sys.argv)[1:]
    result = simple_calculator(std_input)