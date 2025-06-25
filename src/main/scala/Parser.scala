import Types.*

import java.nio.file.{Files, Path}
class Parser {

  private def readBinaryFile(path: Path): List[Byte] = {
    Files.readAllBytes(path).toList
  }
  private def toUnsignedInt(s : Pixel) : Int = s & 0xFF

  private def parsePPM(bytes: List[Byte]): Either[String, PPMImage] = {
    val readableData = bytes.map(_.toChar).mkString("")

    val lines = readableData.take(100).split("\n").filter(_.head != '#').map(_.trim).toList

    lines match {
      case "P6" :: dimension :: maxColor :: data =>
        val Array(width, height) = dimension.split(" ").map(_.toInt)
        val maxVal = maxColor.toInt
        val headerSize = readableData.indexOf(maxColor) + maxColor.length + 1
        val pixelData = bytes.drop(headerSize).grouped(3) // Convert flat byte List into RGB tuples
          .map {
            case List(r, g, b) => RGB(toUnsignedInt(r), toUnsignedInt(g), toUnsignedInt(b)) 
            case _ => RGB(toUnsignedInt(0), toUnsignedInt(0), toUnsignedInt(0)) }
          .toList
          .grouped(width)
          .toList

        Right(PPMImage(width, height, maxVal, pixelData))
      case _ => Left("Invalid PPM format")
    }
  }

  def apply(path: Path)(name:String): Either[String, PPMImage] = {
    val fpath = path.resolve(name + ".ppm")
    parsePPM(readBinaryFile(fpath))
  }
}
