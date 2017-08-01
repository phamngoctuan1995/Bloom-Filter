import scala.math._

/**
  * Created by CPU10816-local on 7/28/2017.
  */
class ScalableBloomFilter[T](maxElement: Long = 5000000000l, probFalsePositive: Double = 0.001, maxNumFilter: Int = 5, hashFunction: (T) => Long = null) {
    val probPerFilter = probFalsePositive / maxNumFilter
    val growthRatio = 2
    var filterSize = (maxElement / (pow(growthRatio, maxNumFilter) - 1)).toLong
    var countElement: Long = 0
    var filters = List(new BloomFilter[T](filterSize, probPerFilter, hashFunction))

    private def CustomHash_x64[T](element: T): Long = {
        val string = element.toString
        var h = 1125899906842597L // prime
        for (i <- 0 until string.length)
            h = 31 * h + string.charAt(i)
        h
    }

    def add(element: T): Unit = {
        if (countElement >= filterSize)
        {
            if (filters.length > maxNumFilter)
                throw new Exception("Scalable Bloom Filter is full")

            countElement = 0
            filterSize *= growthRatio
            filters = new BloomFilter[T](filterSize, probPerFilter, hashFunction) :: filters
        }

        filters.head.add(element)
        countElement += 1
    }

    def add(elements: List[T]): Unit = {
        elements.foreach(element => add(element))
    }

    def mightContain(element: T): Boolean = {
        filters.foreach(filter => if (filter.mightContain(element)) return true)
        return false
    }
}

object Test {
    def main(args: Array[String]): Unit = {
        val cbl = new ScalableBloomFilter[Int](5000000000l, 0.001, 10)
        cbl.add(1)
        println(cbl.mightContain(1))
        println(cbl.mightContain(2))
    }
}