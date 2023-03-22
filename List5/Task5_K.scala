import scala.collection.mutable.Map
import java.lang.reflect.Type
import scala.reflect.runtime.universe._
import scala.annotation.switch
object Test{
    def main(args: Array[String]): Unit={

        var stream = Array[Any](Car(1),2,Car(1),1,Car(2),1,1,Car(3),3,4,Book(1),Book(1),5,Book(1),Book(3),1,Book(1),2,2,Employee(1),Employee(2),3,3,2,1,Employee(3),1,1,Employee(1),Employee(1),2,2,3,Employee(2),Employee(3))
        var carStream = Array[Car]()
        var numberStream = Array[Int]()
        var bookStream = Array[Book]()
        var empStream = Array[Employee]()

        for(item<-stream){
            
            if(item.isInstanceOf[Car]){
                carStream+:=item.asInstanceOf[Car]
            }
            if(item.isInstanceOf[Int]){
                numberStream+:=item.asInstanceOf[Int]
            }
            if(item.isInstanceOf[Book]){
                bookStream+:=item.asInstanceOf[Book]
            }
            if(item.isInstanceOf[Employee]){
                empStream+:= item.asInstanceOf[Employee]
            }
        }
        val k = 3
        try{
            println(misra_gries[Car](carStream,k))
            println(space_saving[Car](carStream,k))
            println(lossy_counting[Car](carStream,0.25))
            println(misra_gries[Book](bookStream,k))
            println(space_saving[Book](bookStream,k))
            println(lossy_counting[Book](bookStream,0.25))
            println(misra_gries[Int](numberStream,k))
            println(space_saving[Int](numberStream,k))
            println(lossy_counting[Int](numberStream,0.25))
            println(misra_gries[Employee](empStream,k))
            println(space_saving[Employee](empStream,k))
            println(lossy_counting[Employee](empStream,0.25))
        }catch{
            case e: IllegalArgumentException => println(e.getMessage())
        }

    }
    def lossy_counting[T](stream:Array[T],epsilon:Double):Map[T,List[Int]]={
        if(epsilon<=0||epsilon>1){
            throw new IllegalArgumentException("Invalid argument - epsilon")
        }
        var w:Int = (1/epsilon).toInt
        var N:Int = 0
        var bucket_length = stream.length/w
        var buckets: List[Array[T]] = stream.sliding(w, w).toList
        var A: Map[T,List[Int]] = Map()
        for(bucket <- buckets){
            for(item<-bucket){
                if(A.contains(item)){
                    var values = A(item)
                    A(item) = List(values(0) + 1, values(1))
                }else{
                    A+=item->List(1,buckets.indexOf(bucket))
                }
            }
            for(item<-A.keys){
                var values = A(item)
                if((values(0)+values(1))<=buckets.indexOf(bucket)+1){
                    A-=item
                }
            }
        }
        A

    }
    def space_saving[T](stream:Array[T],k:Int): Map[T,Int]={
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
                A.remove(A.minBy(_._2)._1)
                A.update(item,1)
            }
        }
        A
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