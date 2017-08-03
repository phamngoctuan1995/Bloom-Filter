import java.io._

import scala.math._

class BloomFilter[T] (numElements: Long = 50000000, probFalsePositive: Double = 0.001, var hashFunction: (T) => Long = null) {
    private var numBits: Long = ceil((numElements * log(probFalsePositive)) / log(1.0 / (pow(2.0, log(2.0)))))
    private var numHashes = round(log(2.0) * numBits / numElements)
    private var numSlices = numBits / 64 + 1
    private var arrBits = Array.fill(numSlices)(0l)

    if (hashFunction == null)
        hashFunction = CustomHash_x64

    def this(fileName: String, hashFunction: (T) => Long) {
        this(1, 0, hashFunction)
        load(fileName, hashFunction)
    }

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

    private def setBit(index: Long): Unit = {
        val base: Long = index >>> 6
        if (base >= numSlices)
            return
        val offset: Long = 1L << index
        arrBits(base) |= offset
    }

    private def getBit(index: Long): Boolean = {
        val base: Long = index >>> 6
        if (base >= numSlices)
            return false
        val offset: Long = 1L << index
        return (arrBits(base) & offset) != 0
    }

    def add(element: T): Unit = {
        val hash = hashFunction(element)
        val hash1 = hash >>> 32
        val hash2 = (hash << 32) >> 32

        for (i <- 0 to numHashes) {
            val computedHash = hash1 + i * hash2
            setBit((computedHash & Long.MaxValue) % numBits)
        }
    }

    def add(elements: List[T]): Unit = {
        elements.foreach(element => add(element))
    }

    def mightContain(element: T): Boolean = {
        val hash = hashFunction(element)
        val hash1 = hash >>> 32
        val hash2 = (hash << 32) >> 32

        for (i <- 0 to numHashes) {
            val computedHash = hash1 + i * hash2
            if (!getBit((computedHash & Long.MaxValue) % numBits))
                return false
        }
        return true
    }
    
    def store(fileName: String) = {
        val start = System.currentTimeMillis()
        val pw = new PrintWriter(new File(fileName))
        pw.println(numBits)
        pw.println(numHashes)
        pw.println(numSlices)
        arrBits.foreach(num => {
            pw.println(num)
        })
        pw.close()

        println("Store: " + (System.currentTimeMillis() - start) / 1000.0)
    }

    def load(fileName: String, _hashFunction: (T) => Long = null) = {
        val start = System.currentTimeMillis()
        val stream = new FileInputStream(fileName)
        val reader = new BufferedReader(new java.io.InputStreamReader(stream))
        numBits = reader.readLine().toLong
        numHashes = reader.readLine().toInt
        numSlices = reader.readLine().toInt
        arrBits = Array.fill(numSlices)(0L)

        (0 until numSlices).foreach(i => {
            arrBits(i) = reader.readLine().toLong
        })

        if (_hashFunction != null)
            hashFunction = _hashFunction
        else
            hashFunction = CustomHash_x64
        println("Load: " + (System.currentTimeMillis() - start) / 1000.0)
    }
}

object Test1 {
    def main(args: Array[String]): Unit = {
        val cbl = new BloomFilter[Int](10, 0.01)
        cbl.add(1)
        cbl.add(2)
        cbl.add(3)
        cbl.add(4)
        cbl.add(5)
        cbl.add(6)
        cbl.add(7)
        cbl.add(8)
        cbl.add(9)
        cbl.store("store.txt")

        val bl2 = new BloomFilter[Int]("store.txt", null)
        println(bl2.mightContain(1))
        println(bl2.mightContain(2))
        println(bl2.mightContain(4))
        println(bl2.mightContain(5))
        println(bl2.mightContain(7))
        println(bl2.mightContain(9))
        println(bl2.mightContain(10))
        println()

        cbl.load("store.txt")
        println(cbl.mightContain(1))
        println(cbl.mightContain(2))
        println(cbl.mightContain(4))
        println(cbl.mightContain(5))
        println(cbl.mightContain(7))
        println(cbl.mightContain(9))
        println(cbl.mightContain(10))
        println()
    }
}