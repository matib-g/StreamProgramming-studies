import scala.collection._

class cars (val name: String){
  def ReturnName(): String = {
    return name
  }
}

class books (val title: String){
  def ReturnName(): String = {
    return title
  }
}

class employees (val name: String, val LastName: String){
  def ReturnName(): String = {
    return name + " " + LastName
  }
}

object Main {
  //main
  def main(args: Array[String]){
    var B: List[Int] = List(1, 1, 2, 1, 2, 3, 4, 2, 1, 2, 1, 2)
    println(Misra_Gries(B.toStream, 3))
    var car1 = new cars("a")
    var car2 = new cars("b")
    var car3 = new cars("c")
    var car4 = new cars("d")
    var C = List(car1, car1, car2, car1, car2, car3, car4, car2, car1, car2, car1, car2)    
    println(Misra_Gries(C.toStream, 3))
  }

  def Misra_Gries[A](stream: Stream[A], k: Int): mutable.Map[A, Int] = {
    var M: mutable.Map[A, Int] = mutable.Map() //make an empty map

    for(i<-stream){ // go through all the elements in our list
      if(M.contains(i)){ //if the dictionary contains this value add 1 to it
        M(i) = M(i) + 1
      }
      else if(M.size < (k - 1)){ //if it doesnt but we didnt reach maximum map size add new counter
        M += (i -> 1)
      }
      else{ //else decrement all keys by 1 and remove keys that reach value = 0
        for(key<-M.keys){
          M(key) = M(key) - 1
          if(M(key) == 0){
            M -= key
          }
        }
      }
    }

    return M
  }
}