package errors

class InvalidExpression(s: String) extends Exception(s) {}  

class ExistingKey(s: String) extends Exception(s) {} 

class NonExistingKey(s: String) extends Exception(s) {} 

class IncompatibleTypes(s: String) extends Exception(s) {}

class InvalidType(s: String) extends Exception(s) {}

class InvalidOperators(s: String) extends Exception(s) {}