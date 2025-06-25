import Main.*


object MyBarcodes extends App {
  // Puteti modifica "MyBarcodesIn" cu numele folderului de intrare pe care il doriti
  // Puteti modifica "MyBarcodesOutput" cu numele folderului de iesire pe care il doriti
  // Calea folderelor trebuie sa fie relativa la root-ul proiectului
  val barcodes = readBarcodes("MyBarcodesInput", "MyBarcodesOutput")
  barcodes.foreach((fname, barcode) => println(fname + ": " + barcode) )
}


