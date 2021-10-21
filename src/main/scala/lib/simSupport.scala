package lib

object simSupport{
    implicit class BigIntSim(b: BigInt){
        def negate(width:Int):BigInt={
            var res = b
            for(i <- 0 until width){
                if(b.testBit(i))
                    res = res.clearBit(i)
                else
                    res = res.setBit(i)
            }
            res
        }
        def divide(width: Int, count: Int): Array[BigInt]={
            var bStr = b.toString(2)
            if(bStr.length < width * count)
                bStr = "0" * (width * count - bStr.length) + bStr
            bStr.grouped(width).map(BigInt(_, 2)).toArray.reverse
        }
    }
}