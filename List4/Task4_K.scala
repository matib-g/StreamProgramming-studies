import scala.collection.mutable.Map
object Test{
    def main(args: Array[String]): Unit={
        var stream = Array(1,2,3,2,6,7,8,2,2,1,3,3,1,1,3)
        try{
            print(misra_gries[Int](stream,0))
        }catch{
            case e: IllegalArgumentException => println(e.getMessage())
        }
        
        println()
        var carStream = Array[Car](Car(1),Car(2),Car(3),Car(2),Car(1),Car(2),Car(2),Car(2),Car(1),Car(2),Car(3),Car(2))
        try{
            print(misra_gries[Car](carStream,5))
        }catch{
            case e: IllegalArgumentException => println(e.getMessage())
        }
        println()
        var bookStream = Array[Book](Book(1),Book(2),Book(3),Book(5),Book(2),Book(2),Book(2),Book(2),Book(1),Book(2),Book(3),Book(2))
        try{
            print(misra_gries[Book](bookStream,5))
        }catch{
            case e: IllegalArgumentException => println(e.getMessage())
        }
        println()
        var employeeStream = Array[Employee](Employee(5),Employee(1),Employee(3),Employee(4),Employee(6),Employee(1),Employee(1),Employee(1),Employee(1),Employee(2),Employee(3),Employee(2))
        try{
            print(misra_gries[Employee](employeeStream,5))
        }catch{
            case e: IllegalArgumentException => println(e.getMessage())
        }
        

    }

    def misra_gries[T](stream:Array[T],k:Int): Map[T,Int] = {
        if(k<1){
            throw new IllegalArgumentException("Invalid argument - k")
        }
        var A:Map[T,Int] = Map()
        for(item<-stream){
            if(A.contains(item)){
                A.update(item,A(item)+1)
            }else if(A.size<k-1){
                A.update(item,1)
            }else{
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
}

case class Car(val id:Int){
}
case class Book(val id:Int){
}
case class Employee(val id:Int){
}