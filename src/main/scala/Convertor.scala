import Types.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

object Convertor {
  def toGreyScale(ppm : Either[String, PPMImage]) : Either[String, PGMImage] = {
    ppm match {
      case Right(p) => Right(PGMImage(p.width, p.height, p.maxColor, p.pixelData.map(_.map(luminance))))
      case Left(x) => Left(x)
    }

  }

  private def luminance(p : RGB ) : Pixel =
    (p.r * 0.3 + p.g * 0.59 + p.b * 0.11).toInt

  def toBlackAndWhite(pgm: Either[String, PGMImage]): Either[String, PBMImage] = {
    pgm match {
      case Right(p) =>
        val (height, width) = (p.height, p.width)
        val arr = p.pixelData

        // Compute pivot for each quadrant
        def computePivot(matrix: List[List[Int]], startRow: Int, endRow: Int, startCol: Int, endCol: Int): Int = {
          val region = (for {
            row <- startRow until endRow
            col <- startCol until endCol
          } yield matrix(row)(col)).toList

          val least = region.min
          val greatest = region.max
          math.round(least + (greatest - least) * 0.5).toInt
        }

        // Define quadrant boundaries
        val midRow = height / 2
        val midCol = width / 2

        val pivot1 = computePivot(arr, 0, midRow, 0, midCol) // Top-left
        val pivot2 = computePivot(arr, 0, midRow, midCol, width) // Top-right
        val pivot3 = computePivot(arr, midRow, height, 0, midCol) // Bottom-left
        val pivot4 = computePivot(arr, midRow, height, midCol, width) // Bottom-right

        // Apply thresholding based on quadrant-specific pivot
        val newArr = arr.indices.map { row =>
          arr(row).indices.map { col =>
            val pivot = if (row < midRow && col < midCol) pivot1
            else if (row < midRow) pivot2
            else if (col < midCol) pivot3
            else pivot4

            if (arr(row)(col) > pivot) 0 else 1
          }.toList
        }.toList

        Right(PBMImage(width, height, newArr))
      case Left(x) => Left(x)
    }

  }

  private def createFile(path: Path)(header:String, extension:String)(name: String, pixels:String): Unit = {
    val content = header + pixels
    val truePath = path.resolve(name + extension)
    if (!Files.exists(truePath))
      Files.write(truePath, content.getBytes(StandardCharsets.US_ASCII), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
  }

  def createPGMFile(path: Path)(name: String, p : PGMImage) : Unit = {
    val header = "P2\n" + p.width + " " + p.height + "\n" + p.maxColor + "\n"
    val pixels = p.pixelData.map(_.mkString(" ")).mkString("\n")
    val ext = "Grey.pgm"
    createFile(path)(header, ext)(name, pixels)
  }

  def createPBMFile(path: Path)(name: String, p : PBMImage) : Unit = {
    val header = "P1\n" + p.width + " " + p.height + "\n"
    val pixels = p.pixelData.map(_.mkString(" ")).mkString("\n")
    val ext = "Black.pbm"
    createFile(path)(header, ext)(name, pixels)
  }


}
