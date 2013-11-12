package org.coomber.euler

import scala.collection.immutable.Stream

import org.coomber.euler.common._
import scala.collection.mutable

package object common {

  implicit class ExtendedInt(n: Int) {

    private def pow(a: Int, b: Int): Int = Math.pow(a, b).toInt

    def squared: Int = pow(n, 2)

    def cubed: Int = pow(n, 3)

    def isFactorOf(a: Int): Boolean = a % n == 0

    def isFactorOf(a: Double): Boolean = a % n == 0

    def isOdd: Boolean = n % 2 != 0
  }

  /**
   * Generate a Stream of Fibonacci numbers
   */
  lazy val fibs: Stream[Int] = 1 #:: 2 #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }

  // Use sieve
  def isPrime[T <% Long](n: T): Boolean = BigInt(n).isProbablePrime(Int.MaxValue)

  def isPrimeSieve(n: Int): Boolean = primesToTwoMillion(n)

  /**
   * Calculate the GCD of two positive natural numbers using the Euclidean Algorithm
   */
  def gcd(a: Int, b: Int): Int = {
    require(a > 0 && b > 0)

    a == b match {
      case true => a
      case false => gcd(Math.min(a, b), Math.max(a, b) - Math.min(a, b))
    }
  }

  /**
   * Find least common multiple in a sequence of Ints
   */
  def lcm(s: Seq[Int]): Int = {
    // Our example goes beyond Int.MaxValue - the workaround could be more thoughtful
    def lcm2(a: Int, b: Int): Int = ((a.toLong * b.toLong) / gcd(a, b)).toInt  // solve for two Ints...
    s.fold(1) { lcm2 }  // ... and sequence with next Int must be multiple of that
  }

  def coprime(a: Int, b: Int): Boolean = gcd(a, b) == 1

  /**
   * Generate Pythagorean triplet using Euclid's formula
   */
  def generateTriple(m: Int, n: Int, k: Int): (Int, Int, Int) = {
    require(m > n)
    require((m - n).isOdd)
    require(coprime(m, n))

    val a = k * (m.squared - n.squared)
    val b = k * 2 * m * n
    val c = k * (m.squared + n.squared)

    (a, b, c)
  }

  /**
   * Tried a different approach - Traversable instead of Stream
   */
  def generatePrimes: Traversable[Long] = {
    val candidates = new Traversable[Long] {
      def foreach[U](f: Long => U): Unit = {
        def next(a: Long, b: Long): Unit = {
          f(a)
          next(b, b + 2)
        }

        next(2, 3)
      }

    }.view

    candidates.filter(isPrime(_))
  }

  def generatePrimesSieve: Stream[Int] = 2 #:: Stream.from(3, 2).filter(isPrimeSieve)

  lazy val primesToTwoMillion = primeSieve(2000003) // Some funcs look for primes < n, which means they need one more

  def primeSieve(size: Int): IndexedSeq[Boolean] = {
    val sieve = mutable.ArraySeq.fill[Boolean](size + 1)(true)

    for(p <- sieve.indices
        if p > 1
        if sieve(p)) {
      for(i <- (p * 2) to(size, p)) {
        sieve.update(i, false)
      }
    }

    sieve
  }
}

object P1to10 {

  def findSumOfMultiplesOf3Or5(limit: Int): Int = {
    (1 until limit).filter(n => 3.isFactorOf(n) || 5.isFactorOf(n)).sum
  }

  def primeFactorsOf(n: Double): List[Int] = {
    val limit = Math.sqrt(n)
    require(limit < Int.MaxValue)  // Candidates generated using Ints below - sufficient for problem

    def primeCandidates = 2 #:: Stream.from(3, 2)  // Candidates are 2 + odd numbers

    primeCandidates.takeWhile(_ < limit).filter(c => c.isFactorOf(n) && isPrimeSieve(c)).toList
  }

  def largestPalindromicNumber(digits: Int): Option[(Int, Int, Int)] = {
    val min = Math.pow(10, digits - 1).toInt
    val max = Math.pow(10, digits).toInt - 1

    for(halfPalindrome <- max to(min, -1)) {
      val palindrome = (halfPalindrome + halfPalindrome.toString.reverse).toInt
      for(a <- max to(min, -1)) {
        for(b <- max to(min, -1)) {
          if(a * b == palindrome) {
            return Some((a, b, palindrome))
          }
        }
      }
    }

    None
  }

  /**
   * First attempt, takes 30s
   */
  def smallestNumberDivisibleBySlow(s: Seq[Int], start: Int = 0): Int =
    Stream.from(1).filter(n => s.forall(_.isFactorOf(n))).take(1).toList.head

  /**
   * After getting a steer on-line
   */
  def smallestNumberDivisibleBy(s: Seq[Int]): Long = lcm(s.toList)

  def sumSquares(s: Seq[Int]): Int = s.map(_.squared).sum

  def squareSums(s: Seq[Int]): Int = s.sum.squared

  def diffSumSquaresSquareSums(s: Seq[Int]): Int = squareSums(s) - sumSquares(s)

  // Could stream from 3 step 2, and take (n - 1), but only marginal gain and makes code less clear
  def nthPrime(n: Int): Int = Stream.from(2).filter(isPrimeSieve).take(n).last

  def largestProductOfNDigits(input: String, n: Int): Int = {
    input.sliding(n).map { sliceOfNDigits =>
      sliceOfNDigits.map( eachChar =>
        eachChar.asDigit).product
    }.max
  }

  // TODO: Bounds can be more refined
  def findTriple(total: Int): Option[(Int, Int, Int)] = {
    for(n <- 1 to 999) {
      for(m <- (n + 1) to 999) {
        for(k <- 1 to 999) {
          if((m - n).isOdd && coprime(m, n)) {
            val (a, b, c) = generateTriple(m, n, k)
            if(a + b + c == total) return Some(a, b, c)
          }
        }
      }
    }

    None
  }

  def sumOfPrimesBelow(n: Int): Long = {
    generatePrimesSieve.takeWhile(_ < n).foldLeft[Long](0L) { _ + _ }
  }
}
