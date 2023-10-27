package errors

class InvalidExpression(s: String) extends Exception(s) {}  

class ExistingKey(s: String) extends Exception(s) {} 

class NonExistingKey(s: String) extends Exception(s) {} 

class IncompatibleTypes(s: String) extends Exception(s) {}