import scala.collection.mutable.Map

case class Car(val id:Int){}
case class Book(val id:Int){}
case class Employee(val id:Int){}

object Test{

    def MisraGries[T](stream:Array[T],k:Int): Map[T,Int] = {
        if (k<1){
            throw new IllegalArgumentException("k: invalid argument")
        }
        var A:Map[T,Int] = Map()
        for(item <- stream){
            if(A.contains(item)){
                A.update(item,A(item)+1)
            }
            else if(A.size<k-1){
                A.update(item,1)
            }
            else{
                var keys = A.keys
                for( key<-keys){
                    A.update(key,A(key)-1)
                    if(A(key)==0){
                        A.remove(key)
                    }
                }
            }
        }
        A
    }

    def main(args: Array[String]): Unit={
        var stream = Array(1,3,3,2,6,7,8,2,2,1,9,3,1,7,3)
        var k:Int = 5
        println()
        try{
            print(MisraGries[Int](stream,0))
        }
        catch{
            case e: IllegalArgumentException => println(e.getMessage())
        }
        
        println()
        var cStream = Array[Car](Car(1),Car(4),Car(3),Car(2),Car(1),Car(2),Car(2),Car(2),Car(1),Car(2),Car(3),Car(2))
        try{
            println(MisraGries[Car](cStream,k))
        }catch{
            case e: IllegalArgumentException => println(e.getMessage())
        }
        var bStream = Array[Book](Book(1),Book(2),Book(3),Book(5),Book(2),Book(5),Book(2),Book(2),Book(1),Book(2),Book(3),Book(2))
        try{
            println(MisraGries[Book](bStream,k))
        }catch{
            case e: IllegalArgumentException => println(e.getMessage())
        }
        var eStream = Array[Employee](Employee(5),Employee(1),Employee(3),Employee(4),Employee(6),Employee(1),Employee(1),Employee(1),Employee(1),Employee(2),Employee(3),Employee(2))
        try{
            println(MisraGries[Employee](eStream,k))
        }catch{
            case e: IllegalArgumentException => println(e.getMessage())
        }
        println()
    }
}

