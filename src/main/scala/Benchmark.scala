/**
  * Created by CPU10816-local on 7/31/2017.
  */
import java.sql.{Connection,DriverManager}

object Benchmark {
    def getUserId(): Array[String] = {
        val url = "jdbc:mysql://127.0.0.1:3306/testdb?characterEncoding=UTF-8"
        val driver = "com.mysql.jdbc.Driver"
        val username = "root"
        val password = "123456"
        var connection:Connection = null
        var res = List.empty[String]
        try {
            Class.forName(driver)
            connection = DriverManager.getConnection(url, username, password)
            val statement = connection.createStatement
            val rs = statement.executeQuery("SELECT uid FROM bloomtest")
            while (rs.next) {
                val user = rs.getString("uid")
                res ::= user
            }
        } catch {
            case e: Exception => e.printStackTrace
        }
        connection.close
        res.toArray
    }

    def testCoutingBloom(train: Array[String], test: Array[String]) = {
        println("Counting Bloom Filter")
        val bf = new CountingBloomFilter[String](train.length, 0.001, 1)
        val start = System.currentTimeMillis()
        bf.add(train.toList)
        println("add time: " + (System.currentTimeMillis() - start) / 1000.0)
        val setTrain = train.toSet
        var falsePos = 0
        var falseNeg = 0
        var countTrue = 0
        var countFalse = 0

        test.foreach(element => {
            if (bf.mightContain(element) > 0) {
                countTrue += 1
                if (setTrain.contains(element) == false)
                    falsePos += 1
            }
            else {
                countFalse += 1
                if (setTrain.contains(element))
                    falseNeg += 1
            }

        })

        println("Total: " + test.length)
        println("faslePos: " + falsePos)
        println("falseNeg: " + falseNeg)
        println("Total time: " + (System.currentTimeMillis() - start) / 1000.0)
        println()
    }

    def testStaticBloom(train: Array[String], test: Array[String]) = {
        println("Static Bloom Filter")
        val bf = new BloomFilter[String](train.length, 0.001)
        val start = System.currentTimeMillis()
        bf.add(train.toList)
        println("add time: " + (System.currentTimeMillis() - start) / 1000.0)
        val setTrain = train.toSet
        var falsePos = 0
        var falseNeg = 0
        var countTrue = 0
        var countFalse = 0

        test.foreach(element => {
            if (bf.mightContain(element)) {
                countTrue += 1
                if (setTrain.contains(element) == false)
                    falsePos += 1
            }
            else {
                countFalse += 1
                if (setTrain.contains(element))
                    falseNeg += 1
            }

        })

        println("Total: " + test.length)
        println("faslePos: " + falsePos)
        println("falseNeg: " + falseNeg)
        println("Total time: " + (System.currentTimeMillis() - start) / 1000.0)
        println()
    }

    def testScalableBloom(train: Array[String], test: Array[String]) = {
        println("Scalable Bloom Filter")
        val bf = new ScalableBloomFilter[String](train.length, 0.001, 5)
        val start = System.currentTimeMillis()
        bf.add(train.toList)
        println("add time: " + (System.currentTimeMillis() - start) / 1000.0)
        val setTrain = train.toSet
        var falsePos = 0
        var falseNeg = 0
        var countTrue = 0
        var countFalse = 0

        test.foreach(element => {
            if (bf.mightContain(element)) {
                countTrue += 1
                if (setTrain.contains(element) == false)
                    falsePos += 1
            }
            else {
                countFalse += 1
                if (setTrain.contains(element))
                    falseNeg += 1
            }

        })

        println("Total: " + test.length)
        println("faslePos: " + falsePos)
        println("falseNeg: " + falseNeg)
        println("Total time: " + (System.currentTimeMillis() - start) / 1000.0)
        println()
    }

    def main(args: Array[String]): Unit = {
        val data = getUserId()
        val (train, test) = data.splitAt((data.length / 2).toInt)
        testStaticBloom(train, test)
        testCoutingBloom(train, test)
        testScalableBloom(train, test)
    }
}
