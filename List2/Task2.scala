import Array._
class PascalTriangleRow(val n:Int){

  private var table = ofDim[Int](n+1, n+1)

  def calculate(): Array[Array[Int]] ={
    if(n >= 0) {
        for(i <- 0 to n){
          table(i)(0) = 1
          table(i)(i) = 1
          for(j <- 1 to (i-1)){
            table(i)(j) = table(i-1)(j-1) + table(i-1)(j)
          }
      }
      return table
    }
    else {
      println(n + " - out of range number")
    }
    return null
  }

  def factor(m : Int) :Int={
    return table(n)(m)
  }
}

object Test2 {
  def main(args: Array[String]): Unit ={
      try{
        val pascal = new PascalTriangleRow(args(0).toInt)
        pascal.calculate()
        for(arg <- args.drop(1)) {
          try {
            println(arg + " - " + pascal.factor(arg.toInt))
          }
          catch{
            case e: ArrayIndexOutOfBoundsException=> println(arg + " - out of range number")
            case e: NumberFormatException => println(arg +  " - invalid argument")
          }
        }
      }
      catch{
        case e: NumberFormatException => println(args(0) +  " - invalid argument")
        case e: NegativeArraySizeException => println(args(0) + " - invalid row number")
        case e: ArrayIndexOutOfBoundsException => println(args(0) +  " - invalid row number")
      }
      }
  }
