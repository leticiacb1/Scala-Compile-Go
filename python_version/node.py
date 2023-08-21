class Node():

    def __init__(self , value):
    
        self.children_left = None
        self.children_right = None
        self.value = value
        self.type = value

    def put_children(self, left = None , right = None):
        if (left != None and right != None):
            self.children_left = left
            self.children_right = right

    def calculate_value(self):
        '''
            Calcula valor do nó
                - Caso seja um nó numérico o valor do nó é o próprio número.
                - Caso o nó seja um operador, a operação entre os filhos é o valor do nó

                          -      --> self.value = value(+) - 5 = 25 
                        /   \
self.value = 30 -->  +       5        
                    /  \
                 10     20

        '''
        if (self.value.isdigit()):
            value = int(self.value)
            return value
        else:
            if (self.value == '+'):    
                value = self.children_left.calculate_value() + self.children_right.calculate_value()
            else:
                value = self.children_left.calculate_value() - self.children_right.calculate_value()

            return int(value)