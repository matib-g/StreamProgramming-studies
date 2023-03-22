import Array._

abstract class Figure {
  def perimeter (): Double
  def field (): Double
}

abstract class Quadrangle ( val side1:Double, 
                            val side2:Double, 
                            val side3:Double, 
                            val side4:Double, 
                            val angle:Double) extends Figure {
}

class Square (override val side1:Double,
              override val side2:Double,
              override val side3:Double,
              override val side4:Double,
              override val angle:Double) extends Quadrangle (side1:Double,
                                                            side2:Double,
                                                            side3:Double,
                                                            side4:Double,
                                                            angle:Double) {
  override def perimeter (): Double = {
    4 * side1
  }
  override def field (): Double = {
    side1 * side1
  }
}

class Rectangle ( override val side1:Double,
                  override val side2:Double,
                  override val side3:Double,
                  override val side4:Double,
                  override val angle:Double) extends Quadrangle (side1:Double,
                                                                side2:Double,
                                                                side3:Double,
                                                                side4:Double,
                                                                angle:Double) {
  override def perimeter (): Double = {
    2 * (side1 + side3)
  }
  override def field (): Double = {
    side1 * side3
  }
}

class Rhombus ( override val side1:Double,
                override val side2:Double,
                override val side3:Double,
                override val side4:Double,
                override val angle:Double) extends Quadrangle (side1:Double,
                                                              side2:Double,
                                                              side3:Double,
                                                              side4:Double,
                                                              angle:Double) {
  override def perimeter (): Double = {
    4 * side1
  }
  override def field (): Double = {
    side1*side1*Math.sin(angle.toRadians)
  }
}

class Circle (val radius: Double) extends Figure {
  override def perimeter (): Double = {
    2 * 3.14 * radius
  }
  override def field (): Double = {
    3.14 * radius * radius
  }
}

class Pentagon (val side: Double) extends Figure {
  override def perimeter (): Double = {
    5 * side
  }
  override def field (): Double = {
    1.72 * side * side
  }
}

class Hexagon (val side: Double) extends Figure {
  override def perimeter (): Double = {
    6 * side
  }
  override def field (): Double = {
    2.598 * side * side
  }
}

object Test {
  def main(args: Array[String]): Unit ={
    var figures = args(0).toCharArray()
    var i = 0;

    for (figure <- figures){
      try {
        if (figure == 'c'){
          i += 1
          var rad = args(i).toDouble
          if (rad <= 0){
            println("The radius value must be positive!")
          }
          else{
            var fig = new Circle(rad)
            println("Circle:")
            println("\tRadius: " + fig.radius)
            println("\tPerimeter: " + fig.perimeter())
            println("\tField: " + fig.field())
          }
        }
        else if (figure == 'p'){
          i += 1
          var side = args(i).toDouble
          var fig = new Pentagon(side)
          println("Pentagon: ")
          println("\tSide: " + fig.side)
          println("\tPerimeter: " + fig.perimeter())
          println("\tField: " + fig.field())
        }
        else if (figure == 'h'){
          i += 1
          var side = args(i).toDouble
          var fig = new Hexagon(side)
          println("Hexagon: ")
          println("\tSide: " + fig.side)
          println("\tPerimeter: " + fig.perimeter())
          println("\tField: " + fig.field())
        }
        else if (figure == 'q'){
          i += 5
          var side1 = args(i-4).toDouble
          var side2 = args(i-3).toDouble
          var side3 = args(i-2).toDouble
          var side4 = args(i-1).toDouble
          var angle = args(i).toDouble
          if (angle == 90){
            if (side1 == side2 && side1 == side3 && side1 == side4){
              var fig = new Square(side1, side2, side3, side4, angle)
              println("Square: ")
              println("\tSide: " + fig.side1)
              println("\tPerimeter" + fig.perimeter())
              println("\tField: " + fig.field())
            }
            else if (side1 == side2 && side3 == side4){
              var fig = new Rectangle(side1, side2, side3, side4, angle)
              println("Rectangle: ")
              println("\tSide 1: " + fig.side1 + " Side 2: " + fig.side3)
              println("\tPerimeter: " + fig.perimeter())
              println("\tField: " + fig.field())
            }
            else {
              println("Figure - " + figure + " : invalid value of sides")
            }
          }
          else if (angle < 90){
            if (side1 == side2 && side1 == side3 && side1 == side4){
              var fig = new Rhombus(side1, side2, side3, side4, angle)
              println("Rhombus: ")
              println("\tSide: " + fig.side1 + "; Angle: " + fig.angle)
              println("\tPerimeters: " + fig.perimeter())
              println("\tField: " + fig.field())
            }
            else {
              println("Figure - " + figure + " : invalid value of sides")
            }
          }
          else {
            println("Figure - " + figure + " : invalid value of angle")
          }
        }
        else {
          println("Figure - " + figure + " : invalid figure type")
        }
      }
      catch{
        case e: NumberFormatException => println("Figure - " + figure + " : argument - " + args(i) +  " - invalid argument")
        case e: IndexOutOfBoundsException => println("Figure - " + figure + " : too few arguments")
      }
    }
  }
}