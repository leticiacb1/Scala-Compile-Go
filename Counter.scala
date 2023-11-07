package id

class Counter {
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
