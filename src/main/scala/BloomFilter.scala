import scala.math._

class BloomFilter[T] (numElements: Long = 50000000, probFalsePositive: Double = 0.001) {
    private val numBits: Long = ceil((numElements * log(probFalsePositive)) / log(1.0 / (pow(2.0, log(2.0)))))
    private val numHashs = round(log(2.0) * numBits / numElements)
    private val numSlices = numBits / 64 + 1
    private val arrBits = Array.fill(numSlices)(0l)

    implicit def longToInt(l: Long): Int = {
        return l.toInt
    }

    implicit def doubleToLong(d: Double): Long = {
        return d.toLong
    }

    private def setBit(index: Long): Unit = {
        val base: Long = index >>> 6
        if (base >= numSlices)
            return
        val offset: Long = 1 << index
        arrBits(base) |= offset
    }

    private def getBit(index: Long): Boolean = {
        val base: Long = index >>> 6
        if (base >= numSlices)
            return false
        val offset: Long = 1 << index
        return (arrBits(base) & offset) != 0
    }

    def add(element: T): Unit = {
        val hash = Hasher.getHashCode(element)
        val hash1 = hash >>> 32
        val hash2 = (hash << 32) >> 32

        for (i <- 0 to numHashs) {
            val computedHash = hash1 + i * hash2
            setBit((computedHash & Long.MaxValue) % numBits)
        }
    }

    def add(elements: List[T]): Unit = {
        elements.foreach(element => add(element))
    }

    def mightContain(element: T): Boolean = {
        val hash = Hasher.getHashCode(element)
        val hash1 = hash >>> 32
        val hash2 = (hash << 32) >> 32

        for (i <- 0 to numHashs) {
            val computedHash = hash1 + i * hash2
            if (!getBit((computedHash & Long.MaxValue) % numBits))
                return false
        }
        return true
    }
}

object Test1 {
    def main(args: Array[String]): Unit = {
        val cbl = new BloomFilter[Int](700000000, 0.001)
        cbl.add(1)
        println(cbl.mightContain(1))
        println(cbl.mightContain(2))
    }
}