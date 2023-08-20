def transform_input(string_input):
    '''
        Separa os elementos de forma individual em uma lista
    '''
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

    return elements