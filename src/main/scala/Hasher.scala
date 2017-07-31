import java.security.MessageDigest

import scala.util.hashing.MurmurHash3
/**
  * Created by CPU10816-local on 7/28/2017.
  */
object Hasher {
    def getHashCode[T](element: T) = CustomHash_x64(element)

    def MurmurHash_x64[T](element: T): Long = {
        val data = element.toString.getBytes
        val length = data.length
        val seed = 0xe17a1465
        val m = 0xc6a4a7935bd1e995L
        val r = 47

        var h = (seed & 0xffffffffl) ^ (length * m)

        var length8 = length / 8

        for (i <- 0 until length8) {
            val i8 = i * 8
            var k =  (data(i8 + 0).toLong & 0xff)    + ((data(i8 + 1).toLong & 0xff) << 8)
            +((data(i8 + 2).toLong & 0xff) << 16) + ((data(i8 + 3).toLong & 0xff) << 24)
            +((data(i8 + 4).toLong & 0xff) << 32) + ((data(i8 + 5).toLong & 0xff) << 40)
            +((data(i8 + 6).toLong & 0xff) << 48) + ((data(i8 + 7).toLong & 0xff) << 56)

            k *= m
            k ^= k >>> r
            k *= m

            h = h ^ k
            h *= m
        }

        length % 8 match {
            case 7 => h = h ^ (data((length & ~7)+6).toLong & 0xff) << 48
            case 6 => h = h ^ (data((length & ~7)+5).toLong & 0xff) << 40
            case 5 => h = h ^ (data((length & ~7)+4).toLong & 0xff) << 32
            case 4 => h = h ^ (data((length & ~7)+3).toLong & 0xff) << 24
            case 3 => h = h ^ (data((length & ~7)+2).toLong & 0xff) << 16
            case 2 => h = h ^ (data((length & ~7)+1).toLong & 0xff) << 8
            case 1 => h = h ^ (data(length & ~7).toLong & 0xff)
                h *= m
            case _ =>
        }

        h = h ^ h >>> r
        h *= m
        h = h ^ h >>> r

        return h
    }

    def MurmurHash_x32_native[T](element: T): Long = MurmurHash3.bytesHash(element.toString.getBytes())

    def JavaHash[T](hashType: String)(element: T): Long = {
        return MessageDigest.getInstance(hashType).digest(element.toString.getBytes).toList.hashCode()
    }

    def NativeHash_x32[T](element: T) = element.toString.hashCode

    def CustomHash_x64[T](element: T): Long = {
        val string = element.toString
        var h = 1125899906842597L // prime
        for (i <- 0 until string.length)
            h = 31 * h + string.charAt(i)
        h
    }
}
