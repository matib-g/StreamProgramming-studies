object HelloScala{
    def printArray(arr:Array[Int]):Unit={
        for(i<-arr){
            print(" "+i)
        }
        println()
    }
    def main(args: Array[String]):Unit = {
        

        var bloomFilterSize = 5
        var numberOfHashFunctions = 3
        var stream =  Array(7, 3, 3, 7, 7, 1)
        var bloomFilter = Array.fill(bloomFilterSize){0}
        printArray(stream)
        println("INSERTING")
        printArray(bloomFilter)
        for(item<-stream){
            println(item)
            for(i <- 1 to numberOfHashFunctions){
                bloomFilter(hash(item,bloomFilterSize,i))+=1
            }
            printArray(bloomFilter)
        }
        println("DELETING")
        for(item<-stream){
            printArray(bloomFilter)
            println(item)
            for(i <- 1 to numberOfHashFunctions){
                bloomFilter(hash(item,bloomFilterSize,i))-=1
            }
            
        }
        printArray(bloomFilter)
        
    }
    def hash(element:Int,n:Int,i:Int):Int={
        (i*element)%n
    }
}