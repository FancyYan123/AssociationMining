import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex

class CustomerTransactionsReader(customerPath:String) {
  val customerFile:BufferedSource = Source.fromFile(customerPath, "utf-8")
  var lines:Array[String] = customerFile.getLines.toArray
  var transactions:List[Array[String]] = List[Array[String]]()

  val customerPattern:Regex = "(?<=\\{)[^}]*(?=\\})".r
  for(line<-lines){
    var allitems = (customerPattern findAllIn line).mkString
    var items:Array[String] = allitems.split(",")
    items = items.sorted
    transactions = items::transactions
  }
}

class UnixTransactionsReader(unixUserPath:String) {
  val userFile:BufferedSource = Source.fromFile(unixUserPath)
  var lines:Array[String] = userFile.getLines().toArray
  var transactions:List[Array[String]] = List[Array[String]]()

  var currentTransaction:List[String] = Nil
  val argsPattern:Regex = "<[0-9]+>".r

  for(line<-lines if line != "**SOF**"){
    currentTransaction = line match {
      case "**SOF**" => currentTransaction
      case "**EOF**" => transactions = currentTransaction.toArray::transactions; Nil
      case _ => if(argsPattern.findAllIn(line).mkString.length == 0) line::currentTransaction else currentTransaction
    }
  }

}

object M{
  def main(args: Array[String]): Unit = {
    var t = new UnixTransactionsReader("C:\\tmp\\DMas2\\UNIX_usage\\USER1\\sanitized_all.981115184025")
    println("bingo")
  }
}