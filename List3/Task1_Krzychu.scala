import Array._
abstract class Figure{
    def perimeter(): Double
    def area():Double
}
abstract class Quadrangle(val side1:Double,val side2:Double,val side3:Double,val side4:Double,val angle:Double) extends Figure{
}
class Square(override val side1:Double,override val side2:Double,override val side3:Double,override val side4:Double,override val angle:Double) extends Quadrangle (side1:Double,side2:Double,side3:Double,side4:Double,angle:Double) {
    override def perimeter(): Double = {
        4*side1
    }
    override def area(): Double = {
        side1*side1
    }
}
class Rectangle(override val side1:Double,override val side2:Double,override val side3:Double,override val side4:Double,override val angle:Double) extends Quadrangle (side1:Double,side2:Double,side3:Double,side4:Double,angle:Double) {
    override def perimeter(): Double = {
        2*(side1+side3)
    }
    override def area(): Double = {
        side1*side3
    }
}
class Rhombus(override val side1:Double,override val side2:Double,override val side3:Double,override val side4:Double,override val angle:Double) extends Quadrangle (side1:Double,side2:Double,side3:Double,side4:Double,angle:Double) {
    override def perimeter(): Double = {
        4*side1
    }
    override def area(): Double = {
        side1*side1*Math.sin(angle.toRadians)
    }
}
class Circle(val radius: Double) extends Figure{
    override def perimeter(): Double = {
        2*3.14*radius
    }
    override def area(): Double = {
        3.14*radius*radius
    }
}
class Pentagon(val side: Double) extends Figure{
    override def perimeter(): Double = {
        5*side
    }
    override def area(): Double = {
        1.72*side*side
    }
}
class Hexagon(val side: Double) extends Figure{
    override def perimeter(): Double = {
        6*side
    }
    override def area(): Double = {
        2.598*side*side
    }
}

object Test {
    def main(args: Array[String]): Unit ={
        var figs = args(0).toCharArray()
        var i = 0;
        for(fig <-figs){
            try{
                if(fig=='c'){
                    i+=1
                    var r = args(i).toDouble
                    var figure = new Circle(r)
                    println("Circle:")
                    println("\tRadius: "+figure.radius)
                    println("\tPerimeter: "+figure.perimeter())
                    println("\tArea: "+figure.area())
                }else if(fig=='p'){
                    i+=1
                    var side = args(i).toDouble
                    var figure = new Pentagon(side)
                    println("Pentagon:")
                    println("\tSide: "+figure.side)
                    println("\tPerimeter: "+figure.perimeter())
                    println("\tArea: "+figure.area())
                }else if(fig=='h'){
                    i+=1
                    var side = args(i).toDouble
                    var figure = new Hexagon(side)
                    println("Hexagon:")
                    println("\tSide: "+figure.side)
                    println("\tPerimeter: "+figure.perimeter())
                    println("\tArea: "+figure.area())
                }
                else if(fig=='q'){
                    i+=5
                    var side1 = args(i-4).toDouble
                    var side2 = args(i-3).toDouble
                    var side3 = args(i-2).toDouble
                    var side4 = args(i-1).toDouble
                    var angle = args(i).toDouble
                    if(angle==90){
                        if(side1==side2&&side1==side3&&side3==side4){
                            var figure = new Square(side1,side2,side3,side4,angle)
                            println("Square:")
                            println("\tSide: "+figure.side1)
                            println("\tPerimeter: "+figure.perimeter())
                            println("\tArea: "+figure.area())
                        }else if(side1==side2&&side3==side4){
                            var figure = new Rectangle(side1,side2,side3,side4,angle)
                            println("Rectangle:")
                            println("\tSide A: "+figure.side1+" Side B: "+figure.side3)
                            println("\tPerimeter: "+figure.perimeter())
                            println("\tArea: "+figure.area())
                        }else{
                            println("Figure - "+fig+" || invalid sides")
                        }
                    }else if(angle<90){
                        if(side1==side2&&side1==side3&&side3==side4){
                            var figure = new Rhombus(side1,side2,side3,side4,angle)
                            println("Rhombus:")
                            println("\tSide: "+figure.side1+" Angle: "+figure.angle)
                            println("\tPerimeter: "+figure.perimeter())
                            println("\tArea: "+figure.area())
                        }else{
                            println("Figure - "+fig+" || invalid sides")
                        }
                    }else{
                        println("Figure - "+fig+" || invalid angle")
                    }
                }else{
                    println("Figure - "+fig+" || invalid figure type")
                }
            }catch
            {
                case e:NumberFormatException => println("Figure - "+fig+" || argument - "+args(i) +  " - invalid argument")
                case e:IndexOutOfBoundsException => println("Figure - "+fig+" || too few arguments")
            }
            
            
        }

        
    }
}
