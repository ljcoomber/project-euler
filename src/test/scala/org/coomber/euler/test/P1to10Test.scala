package org.coomber.euler.test

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner

import scala.collection.immutable.Stream

import org.coomber.euler.common._

import org.coomber.euler.P1to10._

@org.junit.runner.RunWith(classOf[JUnitRunner])
class P1to10Test extends FunSpec with ShouldMatchers {

  def time[R](label: String)(block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    println(label + ": " + (t1 - t0) + "ms")
    result
  }

  // TODO: Break into Package Object test in separate file
  describe("Euler common package object") {
    it("should extend Int with squared function") {
      4.squared should be (16)
    }

    it("should extent Int with a cubed function") {
      3.cubed should be (27)
    }

    it("should extend Int with isFactorOf") {
      2.isFactorOf(3) should be (false)
      2.isFactorOf(4) should be (true)
      2.isFactorOf(4.0) should be (true)
    }

    it("should extend Int with isOdd") {
      2.isOdd should be (false)
      3.isOdd should be (true)
      4.isOdd should be (false)
      5.isOdd should be (true)
    }

    it("should generate a stream of Fibonacci numbers") {
      fibs.take(10) should be (List(1, 2, 3, 5, 8, 13, 21, 34, 55, 89))
    }

    it("should calculate the GCD of two natural numbers") {
      gcd(49, 21) should be (7)
      gcd(14, 15) should be (1)
    }

    it("should calculate the LCM of a sequence of numbers") {
      lcm(1 to 2) should be (2)
      lcm(1 to 3) should be (6)
      lcm(1 to 4) should be (12)
      lcm(1 to 5) should be (60)
      lcm(1 to 6) should be (60)
    }

    it("should determine if two numbers are co-prime") {
      coprime(14, 15) should be (true)
      coprime(14, 21) should be (false)
    }

    it("should generate Pythagorean triples") {
      generateTriple(2, 1, 1) should be((3, 4, 5))
    }

    it("should generate primes") {
      generatePrimes.take(5).toList should be (List(2, 3, 5, 7, 11))
    }

    it("should sieve primes") {
      primesToTwoMillion(2) should be (true)
      primesToTwoMillion(3) should be (true)
      primesToTwoMillion(5) should be (true)
      primesToTwoMillion(7) should be (true)
      primesToTwoMillion(11) should be (true)
      primesToTwoMillion(13) should be (true)
      primesToTwoMillion(17) should be (true)

      primesToTwoMillion(4) should be (false)
      primesToTwoMillion(6) should be (false)
      primesToTwoMillion(8) should be (false)
      primesToTwoMillion(9) should be (false)
      primesToTwoMillion(10) should be (false)
      primesToTwoMillion(12) should be (false)
    }
  }

  describe("Project Euler support functions") {
    it("P1 - should find sum of multiples of 3 or 5") {
      findSumOfMultiplesOf3Or5(10) should equal (23)
    }

    it("P3 - should find the prime factors of a number") {
      primeFactorsOf(13195) should be (List(5, 7, 13, 29))
    }

    it("P4 - should return the largest palindromic number") {
      largestPalindromicNumber(2).get should be ((99, 91, 9009))
    }

    it("P5 - should find the smallest number divisible by all numbers in a range") {
      smallestNumberDivisibleBy(1 to 10) should be (2520)
    }

    it("P6 - should calculate the sum of squares of a list of numbers") {
      sumSquares(1 to 10) should be (385)
    }

    it("P6 - should calculate the square of the sums of a list of numbers") {
      squareSums(1 to 10) should be (3025)
    }

    it("P6 - should calculate the difference between sum of squares and square of sums") {
      diffSumSquaresSquareSums(1 to 10) should be (2640)
    }

    it("P7 - should calculate the nth prime") {
      nthPrime(5) should be(11)
      nthPrime(6) should be(13)
      nthPrime(7) should be(17)
    }

    it("P8 - should find the largest product of n digits") {
      largestProductOfNDigits("123456789", 5) should be (9 * 8 * 7 * 6 * 5)
    }

    it("P10 - should sum primes") {
      sumOfPrimesBelow(10) should be (17)
    }
  }

  describe("Project Euler results") {
    /**
     * If we list all the natural numbers below 10 that are multiples of 3 or 5, we
     * get 3, 5, 6 and 9. The sum of these multiples is 23.
     *
     * Find the sum of all the multiples of 3 or 5 below 1000.
     */
    it("should answer P1") {
      time("P1") { findSumOfMultiplesOf3Or5(1000) should be (233168) }
    }

    /**
     * Each new term in the Fibonacci sequence is generated by adding the previous
     * two terms. By starting with 1 and 2, the first 10 terms will be:
     *
     * 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
     *
     * By considering the terms in the Fibonacci sequence whose values do not
     * exceed four million, find the sum of the even-valued terms.
     */
    it("should answer P2") {
      time("P2") { fibs.takeWhile(_ < 4000000).filter(n => n % 2 == 0).sum should be (4613732)}
    }

    /**
     * The prime factors of 13195 are 5, 7, 13 and 29.
     *
     * What is the largest prime factor of the number 600851475143 ?
     */
     it("should answer P3") {
       time("P3") { primeFactorsOf(600851475143L).max should be (6857)}
     }

    /**
     * A palindromic number reads the same both ways. The largest palindrome made
     * from the product of two 2-digit numbers is 9009 = 91 x 99.
     *
     * Find the largest palindrome made from the product of two 3-digit numbers.
     */
    it("should answer P4") {
      time("P4") { largestPalindromicNumber(3).get should be ((993,913,906609)) }
    }

    /**
     * 2520 is the smallest number that can be divided by each of the numbers from
     * 1 to 10 without any remainder.
     *
     * What is the smallest positive number that is evenly divisible by all of the
     * numbers from 1 to 20?
     */
    it("should answer P5") {
      //time("P5") { smallestNumberDivisibleBySlow(1 to 20, 2520) should be(232792560) }
      time("P5") { smallestNumberDivisibleBy(1 to 20) should be (232792560) }
    }

    /**
     * The sum of the squares of the first ten natural numbers is,
     *
     * 1 2 + 22 + ... + 10 2 = 385
     * The square of the sum of the first ten natural numbers is,

     * (1 + 2 + ... + 10)2 = 55 2 = 3025
     * Hence the difference between the sum of the squares of the first ten natural numbers and the
     * square of the sum is 3025 − 385 =

     * Find the difference between the sum of the squares of the first one hundred natural numbers
     * and the square of the sum.
     */
    it("should answer P6") {
      time("P6") { diffSumSquaresSquareSums(1 to 100) should be (25164150) }
    }

    /**
     * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
     *
     * What is the 10 001st prime number?
     */
    it("should answer p7") {
      time("P7") { nthPrime(10001) should be (104743) }
    }

    /**
     * Find the greatest product of five consecutive digits in the 1000-digit number.
     *
     * <snip>
     */
    val input =  """73167176531330624919225119674426574742355349194934
                    96983520312774506326239578318016984801869478851843
                    85861560789112949495459501737958331952853208805511
                    12540698747158523863050715693290963295227443043557
                    66896648950445244523161731856403098711121722383113
                    62229893423380308135336276614282806444486645238749
                    30358907296290491560440772390713810515859307960866
                    70172427121883998797908792274921901699720888093776
                    65727333001053367881220235421809751254540594752243
                    52584907711670556013604839586446706324415722155397
                    53697817977846174064955149290862569321978468622482
                    83972241375657056057490261407972968652414535100474
                    82166370484403199890008895243450658541227588666881
                    16427171479924442928230863465674813919123162824586
                    17866458359124566529476545682848912883142607690042
                    24219022671055626321111109370544217506941658960408
                    07198403850962455444362981230987879927244284909188
                    84580156166097919133875499200524063689912560717606
                    05886116467109405077541002256983155200055935729725
                    71636269561882670428252483600823257530420752963450"""

    it("should answer P8") {
      time("P8") { largestProductOfNDigits(input, 5) should be (40824)}
    }

    /**
     * A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
     * a^2 + b^2 = c&#94;2
     * For example, 32 + 42 = 9 + 16 = 25 = 52.
     *
     * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
     * Find the product abc.
     */
    it("should answer P9") {
      time("P9") {
        val (a, b, c) = findTriple(1000).get
        a * b * c should be (31875000)
      }
    }

    /**
     * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
     * Find the sum of all the primes below two million.
     */
    it("should answer P10") {
      time("P10") { sumOfPrimesBelow(2000000) should be (142913828922L) }
    }
  }
}