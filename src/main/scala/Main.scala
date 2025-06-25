import Types.*
import Convertor.*
import Decoder.*

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
object Main extends App{
  // Parsing a colour .ppm image (header P6)

  def readBarcodes(ppmSource: String, ppmOutput: String) : List[(String, String)] = {
    val path: Path = Paths.get(".")
    val dir1: Path = path.resolve(ppmSource)
    val ppmExtension = ".ppm"

    if (Files.notExists(dir1)) {
      Files.createDirectories(dir1)
    }
    val fnames: List[String] = Files.list(dir1)
      .iterator()
      .asScala
      .map(_.getFileName.toString)
      .filter(!_.startsWith("."))
      .map(_.replaceFirst("[.][^.]+$", ""))
      .toList
      .sorted

    val p = new Parser().apply(dir1) _
    val PPMImages = fnames.map(p)
    val PGMImages = PPMImages.map(toGreyScale)
    val PBMImages = PGMImages.map(toBlackAndWhite)

    val processedBarcodesPath = path.resolve(ppmOutput)

    if (Files.notExists(processedBarcodesPath)) {
      Files.createDirectories(processedBarcodesPath)
    }

    val pbmCreator = createPBMFile(path.resolve("ProcessedBarcodes"))
    fnames.zip(PBMImages).foreach(pair => pair._2 match {
      case Right(p) => pbmCreator(pair._1, p)
      case _ => None
    })

    fnames.zip(PBMImages.map {
      case Right(img) =>
        val sliceHeight: Int = img.height / 3
        val start: Int = img.height / 2 - sliceHeight / 2
        val end: Int = img.height / 2 + sliceHeight / 2
        val imageCenter = img.pixelData.slice(start, end)
        imageCenter.flatMap(checkRow).flatMap(solve).distinct.headOption.getOrElse("Codul de bare nu a fost identificat.")
      case Left(_) => ""
    })
  }
}
