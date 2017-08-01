import scala.math._

class CountingBloomFilter[T] (numElements: Long = 50000000, probFalsePositive: Double = 0.001, bucketSize: Int = 8, var hashFunction: (T) => Long = null) {
    private val numBuckets: Long = ceil((numElements * log(probFalsePositive)) / log(1.0 / (pow(2.0, log(2.0)))))
    private val numHashs = round(log(2.0) * numBuckets / numElements)
    private val bitPerBucket: Int = if (bucketSize >= 64) 64 else if (bucketSize <= 0) 1 else pow(2, ceil(log(bucketSize) / log(2))).toInt
    private val bucketPerLong = 64 / bitPerBucket
    private val bitLog2 = (log(bucketPerLong) / log(2)).toInt
    private val numSlices = numBuckets / bucketPerLong + 1
    private val arrBuckets = Array.fill(numSlices)(0L)
    private val masks = Array.fill(bucketPerLong)(1L).zipWithIndex.map(mask => ((mask._1 << bitPerBucket) - 1) << (mask._2 * bitPerBucket))
    if (masks.length == 1 && masks(0) == 0)
        masks(0) = -1
    private val offsets = Array.fill(bucketPerLong)(0).zipWithIndex.map(offset => offset._2 * bitPerBucket)
    private var mightFalseNegative = false

    if (hashFunction == null)
        hashFunction = CustomHash_x64

    implicit def longToInt(l: Long): Int = {
        return l.toInt
    }

    implicit def doubleToLong(d: Double): Long = {
        return d.toLong
    }

    private def CustomHash_x64[T](element: T): Long = {
        val string = element.toString
        var h = 1125899906842597L // prime
        for (i <- 0 until string.length)
            h = 31 * h + string.charAt(i)
        h
    }

    private def decreBucket(index: Long): Unit = {
        val base: Long = index >>> bitLog2
        if (base >= numSlices)
            return
        val offset = index & ((1L << bitLog2) - 1)
        var value = (arrBuckets(base) & masks(offset)) >>> offsets(offset)
        if (value == 0)
            return
        value -= 1
        arrBuckets(base) = (arrBuckets(base) & ~masks(offset)) | value << (offset * bitPerBucket)
    }

    private def increBucket(index: Long): Unit = {
        val base: Long = index >>> bitLog2
        if (base >= numSlices)
            return
        val offset = index & ((1L << bitLog2) - 1)
        var value = (arrBuckets(base) & masks(offset)) >>> offsets(offset)
        if (value == masks(0))
        {
            mightFalseNegative = true
            return
        }
        value += 1
        arrBuckets(base) = (arrBuckets(base) & ~masks(offset)) | value << offsets(offset)
    }

    private def getBucket(index: Long): Int = {
        val base: Long = index >>> bitLog2
        if (base >= numSlices)
            return 0
        val offset: Long = index & ((1 << bitLog2) - 1)
        return ((arrBuckets(base) & masks(offset)) >>> offsets(offset)).toInt
    }

    def add(element: T): Unit = {
        val hash = hashFunction(element)
        val hash1 = hash >>> 32
        val hash2 = (hash << 32) >> 32

        for (i <- 0 to numHashs) {
            val computedHash = hash1 + i * hash2
            increBucket((computedHash & Long.MaxValue) % numBuckets)
        }
    }

    def remove(element: T): Unit = {
        val hash = hashFunction(element)
        val hash1 = hash >>> 32
        val hash2 = (hash << 32) >> 32

        for (i <- 0 to numHashs) {
            val computedHash = hash1 + i * hash2
            decreBucket((computedHash & Long.MaxValue) % numBuckets)
        }
    }

    def add(elements: List[T]): Unit = {
        elements.foreach(element => add(element))
    }

    def remove(elements: List[T]): Unit = {
        elements.foreach(element => remove(element))
    }

    def mightContain(element: T): Int = {
        val hash = hashFunction(element)
        val hash1 = hash >>> 32
        val hash2 = (hash << 32) >> 32

        var minValue: Int = Int.MaxValue

        for (i <- 0 to numHashs) {
            val computedHash = hash1 + i * hash2
            val value = getBucket((computedHash & Long.MaxValue) % numBuckets)
            if (value == 0)
                return 0
            if (value < minValue)
                minValue = value
        }
        return minValue
    }
}

object Main {
    def main(args: Array[String]): Unit = {
        val cbl = new CountingBloomFilter[Int](1000, 0.001, 64)
        cbl.add(1)
        println(cbl.mightContain(1))
        cbl.add(2)
        println(cbl.mightContain(2))
        println(cbl.mightContain(3))
        cbl.add(1)
        println(cbl.mightContain(1))
        cbl.add(1)
        println(cbl.mightContain(1))
        cbl.add(1)
        println(cbl.mightContain(1))
        cbl.remove(1)
        println(cbl.mightContain(1))
        cbl.remove(1)
        println(cbl.mightContain(1))
        cbl.remove(1)
        println(cbl.mightContain(1))
    }
}