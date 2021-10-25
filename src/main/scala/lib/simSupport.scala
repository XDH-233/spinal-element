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
        // divide a BigInt num by bit width
        def divide(width: Int, count: Int): Array[BigInt]={
            var bStr = b.toString(2)
            if(bStr.length < width * count)
                bStr = "0" * (width * count - bStr.length) + bStr
            bStr.grouped(width).map(BigInt(_, 2)).toArray.reverse
        }
        // BigInt's toString(2) will truncate heading zeros, so I improve this.
        def toBinaryString(width: Int): String={
            var ret = b.toString(2)
            if(ret.length < width)
                ret = "0" * (width - ret.length) + ret
            ret
        }
        def popCount(width: Int): Int={
            var ret = 0
            for(w <- 0 until width){
                if(b.testBit(w))
                    ret += 1
            }
            ret
        }
    }
    def oneHot(width: Int): BigInt ={
        val ret = BigInt(2).pow(scala.util.Random.nextInt(width))
        ret
    }
}