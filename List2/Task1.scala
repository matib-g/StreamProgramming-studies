class PrimeNumber(val n: Int){

  private var pNumbers = Array[Int](1)
  def calculatePrimeNumbers(): Array[Int] = {
    if(n > 1) {
    for(i <- 2 to n){
      if(i == 2){
        pNumbers(0) = 2
      }
      else if (!(2 until i).exists(divider => i % divider == 0)){
        pNumbers = pNumbers :+ i
        }
      }
    }
    else{
      println(n + " - out of range number")
    }
    return pNumbers
  }

  def pNumber(m: Int) : Int = {
    return pNumbers(m)
  }
}

object Test{
  def main(args: Array[String]): Unit ={
    try{
      val range = new PrimeNumber(args(0).toInt)
      range.calculatePrimeNumbers()
      for (arg <- args.drop(1)){
        try {
          println(arg + " - " + range.pNumber(arg.toInt))
        }
        catch{
          case e: ArrayIndexOutOfBoundsException=> println(arg + " - out of range")
          case e: NumberFormatException => println(arg + " - invalid argument")
        }
      }
    }
    catch{
      case e: NumberFormatException => println(args(0) +  " - invalid argument")
      case e: ArrayIndexOutOfBoundsException => println(args(0) + " - invalid argument")
    }

  }
}
