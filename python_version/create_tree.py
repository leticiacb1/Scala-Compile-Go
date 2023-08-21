from node import Node

def create_tree(elements):
    '''
        Cria arvore e calcula resultado caso a operação seja válida

        Input : 1 + 33 - 5

                    -    ---> value = 34 - 5 ---> result = 29
                  /   \   
value= 1+33 ---> +     5
                / \
               1  33

    '''

    tree = []
    operators__idx = []
    root_tree_idx = 0

    next_element_type = ''

    for idx , var in enumerate(elements):
        
        if(idx == 0):
            
            node = Node(var)
            tree.append(node)
            
            if(var.isdigit()):
                root_tree_idx = idx
                next_element_type = 'operator'
            else:
                raise Exception("Sorry, operation must start with a number")
        
        else:

            if(var in ['+', '-']):
                
                if(next_element_type == 'operator'):
                    # ---- Operador ----
                    node = Node(var)
                    operators__idx.append(idx)

                    # Adiciona filhos
                    # Caso o ultimo elemento seja um operador um erro será lançado tentando buscar o next_node (idx+1)
                    previous_node = tree[root_tree_idx]
                    next_node = Node(elements[idx+1])
                    node.put_children(left = previous_node, right = next_node)

                    # Atualiza ponteiro da raiz:
                    root_tree_idx  = idx

                    tree.append(node)

                    next_element_type = 'number'
                else:
                    raise Exception("Sorry, wrong operation")
            else:
                if(next_element_type == 'number'):
                    # ---- Número ----
                    node = Node(var)
                    tree.append(node)

                    next_element_type = 'operator'
                else:
                    raise Exception("Sorry, wrong operation")

    return tree , root_tree_idx

    