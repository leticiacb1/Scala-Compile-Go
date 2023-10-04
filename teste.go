// Expression calculation test
Println(1)
x = 3+6/3   *  2 -+-  +  2*4/2 + 0/1 -((6+ ((4)))/(2)) // Teste // Teste 2
y_1 = 3
y_1 = y_1 + 2
z__ = x + y_1

Println(x)
Println(z__+1)

// If and else test
if x > 2 {
    Println(1)
}else{
    Println(0)
}

// For test
for i = 0 ; i < 10 ; i = i + 1 {

    if i < 5 {
        Println(0)
    }

    if i == 5 {
        Println(5)
    }
}

for j = 0 ; j < 2 ; j = j + 1 {
    y = Scanln()
    Println(y)
}


// Boolean operations tests
x = 2
y = 1

if x == 2 || y == 2 {
	Println(x)
}

b = 3
c = 1
a = 0
if b == 3 && c == 3 {
    //Não deve printar
    Println(100)
}else{
    if c == 1 || b == 2 {
        // Deve printar
        Println(50)
    }
}
