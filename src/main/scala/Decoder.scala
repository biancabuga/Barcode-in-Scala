import Types.{Bit, Digit, Even, Odd, NoParity, One, Parity, Pixel, Str, Zero}
import scala.collection.immutable

object Decoder {
  // TODO 1.1
  def toBit(s: Char): Bit =
    s match {
      case '0' => Zero
      case '1' => One
    }
  def toBit(s: Int): Bit =
    s match {
      case 0 => Zero
      case 1 => One
    }

  // TODO 1.2
  def complement(c: Bit): Bit =
    c match {
      case Zero => One
      case One => Zero
    }

  // TODO 1.3
  val LStrings: List[String] = List("0001101", "0011001", "0010011", "0111101", "0100011",
    "0110001", "0101111", "0111011", "0110111", "0001011")
  val leftOddList: List[List[Bit]] = LStrings.map(_.map(toBit).toList) // codificări L
  val rightList: List[List[Bit]] = leftOddList.map(_.map(complement)) // codificări R
  val leftEvenList: List[List[Bit]] = leftOddList.map(_.reverse.map(complement))// codificări  G
  
  // TODO 1.4
  def group[A](l: List[A]): List[List[A]] = {
    def op(elem: A, acc: List[List[A]]): List[List[A]] =
      acc match {
        case (headlist :: tail) 
          if headlist.head == elem => (elem :: headlist) :: tail
        case _ => List(elem) :: acc
      }
    l.foldRight(List[List[A]]())(op)
  }
  // TODO 1.5
  def runLength[A](l: List[A]): List[(Int, A)] =
    group(l).map(g => (g.length, g.head))
  
  case class RatioInt(n: Int, d: Int) extends Ordered[RatioInt] {
    require(d != 0, "Denominator cannot be zero")
    private val gcd = BigInt(n).gcd(BigInt(d)).toInt
    val a = n / gcd // numărător
    val b = d / gcd // numitor

    override def toString: String = s"$a/$b"

    override def equals(obj: Any): Boolean = obj match {
      case that: RatioInt => this.a.abs == that.a.abs &&
        this.b.abs == that.b.abs &&
        this.a.sign * this.b.sign == that.a.sign * that.b.sign
      case _ => false
    }

    // TODO 2.1
    def -(other: RatioInt): RatioInt = RatioInt(this.a * other.b - other.a * this.b, this.b * other.b)
    def +(other: RatioInt): RatioInt = RatioInt(this.a * other.b + other.a * this.b, this.b * other.b)
    def *(other: RatioInt): RatioInt = RatioInt(this.a * other.a, this.b * other.b)
    def /(other: RatioInt): RatioInt = RatioInt(this.a * other.b, this.b * other.a)

    // TODO 2.2
    def compare(other: RatioInt): Int = {
      val left = this.a * other.b
      val right = other.a * this.b
      if (left < right) -1
      else if (left > right) 1
      else 0
    }
  }
  
  // TODO 3.1
  def scaleToOne[A](l: List[(Int, A)]): List[(RatioInt, A)] = {
    val totalNumber = l.map(_._1).sum
    def op(pair :(Int, A)): (RatioInt, A) = {
      (RatioInt(pair._1, totalNumber), pair._2)
    }
    l.map(op)
  }

  // TODO 3.2
  def scaledRunLength(l: List[(Int, Bit)]): (Bit, List[RatioInt]) = {
    (l.head._2, scaleToOne(l).map(_._1))
  }
  
  // TODO 3.3
  def toParities(s: Str): List[Parity] = {
    def op(c: Char) : Parity = {
      c match {
        case 'L' => Odd
        case 'G' => Even
        case _ => NoParity
      }
    }
    s.toList.map(op)
  }
  
  // TODO 3.4
  val PStrings: List[String] = List("LLLLLL", "LLGLGG", "LLGGLG", "LLGGGL", "LGLLGG",
    "LGGLLG", "LGGGLL", "LGLGLG", "LGLGGL", "LGGLGL")
  val leftParityList: List[List[Parity]] = PStrings.map(_.toList).map(toParities)

  // TODO 3.5
  type SRL = (Bit, List[RatioInt])
  val leftOddSRL:  List[SRL] = leftOddList.map(bits => scaledRunLength(runLength(bits)))
  val leftEvenSRL:  List[SRL] = leftEvenList.map(bits => scaledRunLength(runLength(bits)))
  val rightSRL:  List[SRL] = rightList.map(bits => scaledRunLength(runLength(bits)))

  // TODO 4.1
  def distance(l1: SRL, l2: SRL): RatioInt = {
    def op(pair: (RatioInt, RatioInt)): RatioInt = {
      val difference = pair._1 - pair._2
      RatioInt(math.abs(difference.a), difference.b)
    }

    if (complement(l1._1) == l2._1) {
      RatioInt(100, 1)
    } else {
      val newList = l1._2.zip(l2._2).map(op)
      newList.reduce(_ + _)
    }
  }
  
  // TODO 4.2
  def bestMatch(SRL_Codes: List[SRL], digitCode: SRL): (RatioInt, Digit) = {
    val index: List[Digit] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    def op(pair: (SRL, Digit)): (RatioInt, Digit) = {
      (distance(pair._1, digitCode), pair._2)
    }
    SRL_Codes.zip(index).map(op).min((a, b) => a._1.compare(b._1))
  }
  
  // TODO 4.3
  def bestLeft(digitCode: SRL): (Parity, Digit) = {
    val (distOdd, digitOdd) = bestMatch(leftOddSRL, digitCode)
    val (distEven, digitEven) = bestMatch(leftEvenSRL, digitCode)

    if (distOdd.compare(distEven) < 0) (Odd, digitOdd)
    else (Even, digitEven)
  }
  
  // TODO 4.4
  def bestRight(digitCode: SRL): (Parity, Digit) = {
    (NoParity, bestMatch(rightSRL, digitCode)._2)
  }

  def chunkWith[A](f: List[A] => (List[A], List[A]))(l: List[A]): List[List[A]] = {
    l match {
      case Nil => Nil
      case _ =>
        val (h, t) = f(l)
        h :: chunkWith(f)(t)
    }
  }
  
  def chunksOf[A](n: Int)(l: List[A]): List[List[A]] =
    chunkWith((l: List[A]) => l.splitAt(n))(l)

  // TODO 4.5
  def findLast12Digits(rle:  List[(Int, Bit)]): List[(Parity, Digit)] = {
    val length = rle.length
    if (length != 59) return Nil
      val left = rle.drop(3).take(24)
      val right = rle.drop(32).take(24)

      val leftChunks = chunksOf(4)(left)
      val rightChunks = chunksOf(4)(right)

      val first6digits = leftChunks.map(chunk => bestLeft(scaledRunLength(chunk)))
      val last6digits = rightChunks.map(chunk => bestRight(scaledRunLength(chunk)))
      first6digits ++ last6digits
  }

  // TODO 4.6
  def firstDigit(l: List[(Parity, Digit)]): Option[Digit] = {
    val parityList = l.take(6).map(_._1)
    leftParityList.zipWithIndex.find(_._1 == parityList).map(_._2)
  }

  // TODO 4.7
  def checkDigit(l: List[Digit]): Digit = {
    val sum = l.zipWithIndex.map{
      case (digit, index) =>
        if (index % 2 == 0) digit * 1
        else digit * 3
    }.sum

    val digit = (10 - (sum % 10)) % 10
    digit
  }
  
  // TODO 4.8
  def verifyCode(code: List[(Parity, Digit)]): Option[String] = {
    if (code.length != 13) None
    else {
      val digits = code.map(_._2)
      val parityDigit = firstDigit(code.drop(1))
      val lastDigit = checkDigit(digits.dropRight(1))
      val actualParityDigit = digits.head
      val actualCheckDigit = digits.reverse.head
      if (parityDigit.isDefined &&
        parityDigit.get == actualParityDigit &&
        lastDigit == actualCheckDigit)
        Some(digits.mkString)
      else None
    }
  }
  
  // TODO 4.9
  def solve(rle:  List[(Int, Bit)]): Option[String] = {
    val digits = findLast12Digits(rle)

    if (digits.length != 12) None
    else {
      firstDigit(digits) match {
        case Some(first) =>
          val code = (NoParity, first) :: digits
          verifyCode(code)
        case None => None
      }
    }
  }
  
  def checkRow(row: List[Pixel]): List[List[(Int, Bit)]] = {
    val rle = runLength(row);

    def condition(sl: List[(Int, Pixel)]): Boolean = {
      if (sl.isEmpty) false
      else if (sl.size < 59) false
      else sl.head._2 == 1 &&
        sl.head._1 == sl.drop(2).head._1 &&
        sl.drop(56).head._1 == sl.drop(58).head._1
    }

    rle.sliding(59, 1)
      .filter(condition)
      .toList
      .map(_.map(pair => (pair._1, toBit(pair._2))))
  }
}


