
object HelloScala{
    def main(args: Array[String]):Unit = {
        for( arg <- args){
            try{
                if(arg.toInt==1) {println(arg + ": Only one divisor of 1 is 1.")};
                else if(arg.toInt==(-1)) {println(arg + ": Only one divisor of -1 is 1.")};
                else if(arg.toInt==0) {println(arg + ": Zero has infinitely many divisors.")};
                else{
                    val divisor : Int = getDivisor(arg.toInt);
                    println(arg + ": " + divisor);
                }
            }catch{
                case exception: NumberFormatException => println(arg + ":  The conversion is impossible!");
            }
            
        }
    }
    def getDivisor(x:Int):Int={
        val y : Int = (x).abs;
            for(i<-2 to y/2){
                if(y%i==0){
                return y/i
                }
            }
        1
    }
}