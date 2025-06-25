
object Types {
  type Pixel = Int
  type Str = List[Char]
  type Digit = Int
  sealed trait Bit
  case object Zero extends Bit
  case object One extends Bit

  sealed trait Parity
  case object Odd extends Parity
  case object Even extends Parity
  case object NoParity extends Parity

  case class RGB(r: Pixel, g:Pixel, b:Pixel)
  case class PPMImage(width: Int, height: Int, maxColor: Int, pixelData: List[List[RGB]])
  case class PBMImage(width: Int, height: Int, pixelData: List[List[Pixel]])
  case class PGMImage(width: Int, height: Int, maxColor: Int, pixelData: List[List[Pixel]])
}
