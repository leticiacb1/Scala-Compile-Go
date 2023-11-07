package id

class Counter {
  // A classe não possui mais a variável counter

  // Métodos específicos para instâncias da classe
  def incrementCounter(): Int = {
    Counter.incrementCounter()
  }

  def getCounter: Int = {
    Counter.getCounter()
  }
}

object Counter {
  private var counter = 0

  def incrementCounter(): Int = {
    counter += 1
    counter
  }

  def getCounter(): Int = {
    counter
  }
}
