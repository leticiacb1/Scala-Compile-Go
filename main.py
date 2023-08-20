import sys 

from transform_input import transform_input
from create_tree import create_tree
 
def main(string_input):
   
    elements = transform_input(string_input) 
    print(elements)   
    tree , root_idx = create_tree(elements)

    # Calcula valor da raíz    
    result = tree[root_idx].calculate_value()
    return result
    
if __name__ == '__main__':
    std_input = (sys.argv)[1:]
    result = main(std_input[0])

    print(f"\n\t Result : {result}\n")