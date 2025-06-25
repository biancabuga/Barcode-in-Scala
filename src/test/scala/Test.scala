import org.scalatest.funsuite.AnyFunSuite
import Decoder.*
import Types.*
import Main.readBarcodes
import org.scalactic.source.Position
import org.scalatest.BeforeAndAfterAll

class Test extends AnyFunSuite with BeforeAndAfterAll {

  var punctaj = 0

  override def beforeAll(): Unit = {
    println("Incepe testarea...")
  }

  override def afterAll(): Unit = {
    println(s"Ai obtinut $punctaj/100 puncte.")
    if (punctaj == 100) println("Felicitari, ai rezolvat tot! Asteptam sa ne explici cum ai facut :)")
  }


  // ----------------------
  //  Task 1
  // ----------------------

  test("Tipul Bit (5p):") {
    assert(toBit(1) == One)
    assert(toBit('1') == One)
    assert(toBit(0) == Zero)
    assert(toBit('0') == Zero)
    assert(complement(Zero) == One)
    assert(complement(One) == Zero)
    punctaj += 5
  }

  test("Codificari (5p):") {
    val rList = List(List(One, One, One, Zero, Zero, One, Zero),
      List(One, One, Zero, Zero, One, One, Zero),
      List(One, One, Zero, One, One, Zero, Zero),
      List(One, Zero, Zero, Zero, Zero, One, Zero),
      List(One, Zero, One, One, One, Zero, Zero),
      List(One, Zero, Zero, One, One, One, Zero),
      List(One, Zero, One, Zero, Zero, Zero, Zero),
      List(One, Zero, Zero, Zero, One, Zero, Zero),
      List(One, Zero, Zero, One, Zero, Zero, Zero),
      List(One, One, One, Zero, One, Zero, Zero))

    val lOddList = List(List(Zero, Zero, Zero, One, One, Zero, One),
      List(Zero, Zero, One, One, Zero, Zero, One),
      List(Zero, Zero, One, Zero, Zero, One, One),
      List(Zero, One, One, One, One, Zero, One),
      List(Zero, One, Zero, Zero, Zero, One, One),
      List(Zero, One, One, Zero, Zero, Zero, One),
      List(Zero, One, Zero, One, One, One, One),
      List(Zero, One, One, One, Zero, One, One),
      List(Zero, One, One, Zero, One, One, One),
      List(Zero, Zero, Zero, One, Zero, One, One))

    val lEvenList = List(List(Zero, One, Zero, Zero, One, One, One),
      List(Zero, One, One, Zero, Zero, One, One),
      List(Zero, Zero, One, One, Zero, One, One),
      List(Zero, One, Zero, Zero, Zero, Zero, One),
      List(Zero, Zero, One, One, One, Zero, One),
      List(Zero, One, One, One, Zero, Zero, One),
      List(Zero, Zero, Zero, Zero, One, Zero, One),
      List(Zero, Zero, One, Zero, Zero, Zero, One),
      List(Zero, Zero, Zero, One, Zero, Zero, One),
      List(Zero, Zero, One, Zero, One, One, One))
    assert(leftOddList == lOddList)
    assert(rightList == rList)
    assert(leftEvenList == lEvenList)
    punctaj += 5
  }

  test("Group and runLength (10p):") {
    val l = List(1, 1, 1, 3, 3, 2, 2, 1)
    assert(group(l) == List(List(1, 1, 1), List(3, 3), List(2, 2), List(1)))
    assert(runLength(l) == List((3, 1), (2, 3), (2, 2), (1, 1)))
    punctaj += 10
  }


  // ----------------------
  //  Task 2
  // ----------------------
  test("Tipul RatioInt (10p):") {
    //    Sanity checks
    assert(RatioInt(1, -2) == RatioInt(-2, 4))
    assert(RatioInt(0, -5) == RatioInt(-0, 3))
    assert(RatioInt(0, 3) == RatioInt(0, 7))

    assert(RatioInt(1, 2) + RatioInt(1, 2) == RatioInt(1, 1))
    assert(RatioInt(1, 2) - RatioInt(1, 4) == RatioInt(1, 4))
    assert(RatioInt(5, 7) * RatioInt(3, 4) == RatioInt(15, 28))
    assert(RatioInt(1, 5) / RatioInt(2, 3) == RatioInt(3, 10))
    assert(RatioInt(1, 2) > RatioInt(1, 4))
    punctaj += 10
  }

  // ----------------------
  //  Task 3
  // ----------------------
  test("Scale to one (10p):") {
    val l = List((3, 1), (2, 3), (2, 2), (1, 1))
    val res = List((RatioInt(3, 8), 1), (RatioInt(1, 4), 3), (RatioInt(1, 4), 2), (RatioInt(1, 8), 1))

    assert(scaleToOne(l) == res)
    punctaj += 10
  }

  test("Scaled run length (10p):") {
    val l = List((3, Zero), (2, One), (2, Zero), (1, One))
    val res = (Zero, List(RatioInt(3, 8), RatioInt(1, 4), RatioInt(1, 4), RatioInt(1, 8)))

    assert(scaledRunLength(l) == res)
    punctaj += 10
  }

  test("Paritati (5p):") {
    assert(toParities("LLLGLGGL".toList) == List(Odd, Odd, Odd, Even, Odd, Even, Even, Odd))

    assert(leftParityList == List(List(Odd, Odd, Odd, Odd, Odd, Odd),
      List(Odd, Odd, Even, Odd, Even, Even),
      List(Odd, Odd, Even, Even, Odd, Even),
      List(Odd, Odd, Even, Even, Even, Odd),
      List(Odd, Even, Odd, Odd, Even, Even),
      List(Odd, Even, Even, Odd, Odd, Even),
      List(Odd, Even, Even, Even, Odd, Odd),
      List(Odd, Even, Odd, Even, Odd, Even),
      List(Odd, Even, Odd, Even, Even, Odd),
      List(Odd, Even, Even, Odd, Even, Odd)))
      punctaj += 5
  }

  test("Codificari cu Run Length Scaled (5p):") {
    assert(leftOddSRL == List((Zero, List(RatioInt(3, 7), RatioInt(2, 7), RatioInt(1, 7), RatioInt(1, 7))),
      (Zero, List(RatioInt(2, 7), RatioInt(2, 7), RatioInt(2, 7), RatioInt(1, 7))),
      (Zero, List(RatioInt(2, 7), RatioInt(1, 7), RatioInt(2, 7), RatioInt(2, 7))),
      (Zero, List(RatioInt(1, 7), RatioInt(4, 7), RatioInt(1, 7), RatioInt(1, 7))),
      (Zero, List(RatioInt(1, 7), RatioInt(1, 7), RatioInt(3, 7), RatioInt(2, 7))),
      (Zero, List(RatioInt(1, 7), RatioInt(2, 7), RatioInt(3, 7), RatioInt(1, 7))),
      (Zero, List(RatioInt(1, 7), RatioInt(1, 7), RatioInt(1, 7), RatioInt(4, 7))),
      (Zero, List(RatioInt(1, 7), RatioInt(3, 7), RatioInt(1, 7), RatioInt(2, 7))),
      (Zero, List(RatioInt(1, 7), RatioInt(2, 7), RatioInt(1, 7), RatioInt(3, 7))),
      (Zero, List(RatioInt(3, 7), RatioInt(1, 7), RatioInt(1, 7), RatioInt(2, 7)))))

    assert(rightSRL == List((One, List(RatioInt(3, 7), RatioInt(2, 7), RatioInt(1, 7), RatioInt(1, 7))),
      (One, List(RatioInt(2, 7), RatioInt(2, 7), RatioInt(2, 7), RatioInt(1, 7))),
      (One, List(RatioInt(2, 7), RatioInt(1, 7), RatioInt(2, 7), RatioInt(2, 7))),
      (One, List(RatioInt(1, 7), RatioInt(4, 7), RatioInt(1, 7), RatioInt(1, 7))),
      (One, List(RatioInt(1, 7), RatioInt(1, 7), RatioInt(3, 7), RatioInt(2, 7))),
      (One, List(RatioInt(1, 7), RatioInt(2, 7), RatioInt(3, 7), RatioInt(1, 7))),
      (One, List(RatioInt(1, 7), RatioInt(1, 7), RatioInt(1, 7), RatioInt(4, 7))),
      (One, List(RatioInt(1, 7), RatioInt(3, 7), RatioInt(1, 7), RatioInt(2, 7))),
      (One, List(RatioInt(1, 7), RatioInt(2, 7), RatioInt(1, 7), RatioInt(3, 7))),
      (One, List(RatioInt(3, 7), RatioInt(1, 7), RatioInt(1, 7), RatioInt(2, 7)))))

    assert(leftEvenSRL == List((Zero, List(RatioInt(1, 7), RatioInt(1, 7), RatioInt(2, 7), RatioInt(3, 7))),
      (Zero, List(RatioInt(1, 7), RatioInt(2, 7), RatioInt(2, 7), RatioInt(2, 7))),
      (Zero, List(RatioInt(2, 7), RatioInt(2, 7), RatioInt(1, 7), RatioInt(2, 7))),
      (Zero, List(RatioInt(1, 7), RatioInt(1, 7), RatioInt(4, 7), RatioInt(1, 7))),
      (Zero, List(RatioInt(2, 7), RatioInt(3, 7), RatioInt(1, 7), RatioInt(1, 7))),
      (Zero, List(RatioInt(1, 7), RatioInt(3, 7), RatioInt(2, 7), RatioInt(1, 7))),
      (Zero, List(RatioInt(4, 7), RatioInt(1, 7), RatioInt(1, 7), RatioInt(1, 7))),
      (Zero, List(RatioInt(2, 7), RatioInt(1, 7), RatioInt(3, 7), RatioInt(1, 7))),
      (Zero, List(RatioInt(3, 7), RatioInt(1, 7), RatioInt(2, 7), RatioInt(1, 7))),
      (Zero, List(RatioInt(2, 7), RatioInt(1, 7), RatioInt(1, 7), RatioInt(3, 7)))))
      punctaj += 5
  }

  // ----------------------
  //  Task 4
  // ----------------------

  test("Distance (5p):") {
    val l1 = (Zero, List(RatioInt(1, 7), RatioInt(1, 7), RatioInt(4, 7), RatioInt(1, 7)))
    val l2 = (Zero, List(RatioInt(3, 7), RatioInt(1, 7), RatioInt(2, 7), RatioInt(1, 7)))
    val l3 = (One, l1._2)

    assert(distance(l1, l2) == RatioInt(4, 7))
    assert(distance(l1, l1) == RatioInt(0, 7))
    assert(distance(l1, l3) > RatioInt(1, 1))
    punctaj += 5
  }

  test("Best match (5p):") {
    val fiveLeftOdd = (Zero, List(RatioInt(4, 30), RatioInt(9, 30), RatioInt(13, 30), RatioInt(4, 30)))
    val oneRight = (One, List(RatioInt(12, 51), RatioInt(15, 51), RatioInt(16, 51), RatioInt(8, 51)))
    val eightLeftEven = (Zero, List(RatioInt(43, 100), RatioInt(12, 100), RatioInt(30, 100), RatioInt(15, 100)))

    //    Best Match
    assert(bestMatch(leftOddSRL, leftOddSRL(5)) == (RatioInt(0, 1), 5))
    assert(bestMatch(leftEvenSRL, leftEvenSRL(3)) == (RatioInt(0, 1), 3))
    assert(bestMatch(rightSRL, rightSRL(7)) == (RatioInt(0, 1), 7))

    assert(bestMatch(rightSRL, leftOddSRL(5))._1 > RatioInt(1, 1))

    assert(bestMatch(leftOddSRL, fiveLeftOdd) == (RatioInt(140, 3675), 5))
    assert(bestMatch(rightSRL, oneRight) == (RatioInt(12852, 127449), 1))
    assert(bestMatch(leftEvenSRL, eightLeftEven) == (RatioInt(4480, 98000), 8))

    //    Best Left
    assert(bestLeft(leftOddSRL(2)) == (Odd, 2))
    assert(bestLeft(leftEvenSRL.head) == (Even, 0))
    assert(bestLeft(fiveLeftOdd) == (Odd, 5))
    assert(bestLeft(eightLeftEven) == (Even, 8))

    //    Best Right
    assert(bestRight(rightSRL(6)) == (NoParity, 6))
    assert(bestRight(oneRight) == (NoParity, 1))
    punctaj += 5
  }

  test("Find last 12 digits (10p):") {
    val code = "5900084249478"
    val codeList = code.toList.map(_.toInt - '0'.toInt)
    val bits = "10100010110100111010011100011010110111001110101010110110010111001110100101110010001001001000101"
    val bitList = bits.map(toBit).toList
    val rle = runLength(bitList)

    val result = findLast12Digits(rle)
    assert(result == List((Odd, 9), (Even, 0), (Even, 0), (Odd, 0), (Odd, 8), (Even, 4), (NoParity, 2), (NoParity, 4), (NoParity, 9), (NoParity, 4), (NoParity, 7), (NoParity, 8)))
    punctaj += 10
  }
  test("First digit (5p):") {
    val result = List((Odd, 9), (Even, 0), (Even, 0), (Odd, 0), (Odd, 8), (Even, 4), (NoParity, 2), (NoParity, 4), (NoParity, 9), (NoParity, 4), (NoParity, 7), (NoParity, 8))
    val first = firstDigit(result).get
    assert(first == 5)
    punctaj += 5
  }
  test("Check digit (1p):") {
    val code = "5900084249478"
    val codeList = code.toList.map(_.toInt - '0'.toInt)
    assert(checkDigit(codeList.dropRight(1)) == 8)
    val code2 = "5901234123457"
    val codeList2 = code2.toList.map(_.toInt - '0'.toInt)
    assert(checkDigit(codeList2.dropRight(1)) == 7)
  }
  test("Verify code (4p):") {
    val result = List((Odd, 9), (Even, 0), (Even, 0), (Odd, 0), (Odd, 8), (Even, 4), (NoParity, 2), (NoParity, 4), (NoParity, 9), (NoParity, 4), (NoParity, 7), (NoParity, 8))
    val first = 5
    val code = "5900084249478"
    val bits = "10100010110100111010011100011010110111001110101010110110010111001110100101110010001001001000101"
    val bitList = bits.map(toBit).toList
    val rle = runLength(bitList)

    val whole = (NoParity, first) :: result
    assert(verifyCode(whole).get == code)
    assert(solve(rle).get == code)
    punctaj += 5
  }

  test("Final (10p):") {
    val barcodes = readBarcodes("InitialBarcodes", "ProcessedBarcodes")
    assert(barcodes.map(_._2) == List("5941845000907", "5941416010601", "5941460010398", "5941867060774", "5900084222501", "6423598347397"))

    println("Codurile de bare citite sunt:")
    barcodes.foreach((fname, barcode) => println(fname + ": " + barcode))
    punctaj += 10
  }
}
