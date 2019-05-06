import scala.collection.mutable.{Map, Set}
import scala.math._

object RuleGenerator{
  def generateRules(frequentItemsets: Map[Int, Map[Set[String], Int]], minConvince:Double, transactionsNum:Int):Array[Rule] = {
    var rules:List[Rule] = List[Rule]()
    var maxItemsetSize = frequentItemsets.keys.max
    var k = maxItemsetSize
    while(k>1){
      val kItemsetsMap = frequentItemsets(k)
      for((itemset, freq)<- kItemsetsMap){
        val support:Double = freq.toDouble/transactionsNum.toDouble
        var allSubsets = getSubsets(itemset)
        for(subset<-allSubsets){
          var freqFactor = frequentItemsets(subset.size)(subset)
          var convince:Double = freq.toDouble/freqFactor.toDouble
          if(convince>minConvince){
            val rule = new Rule(subset, itemset.&~(subset), support, convince)
            rules = rule::rules
          }
        }
      }
      k-=1
    }
    rules.toArray.sortBy(f=> -f.support-f.convince)
  }


  def getSubsets(items:Set[String]):Set[Set[String]] = {
    assert(items.size < 32)
    var itemset = items.toArray
    var maxMark:Int = (pow(2, itemset.length)-1).toInt

    var allSubsets:Set[Set[String]] = Set[Set[String]]()
    var mark:Int = 1
    while(mark < maxMark){
      var subset:Set[String] = Set[String]()
      for(k <- itemset.indices){
        if((mark&(1<<k)) != 0)
          subset.add(itemset(k))
      }
      allSubsets += subset
      mark += 1
    }
    allSubsets
  }

  def main(args: Array[String]): Unit ={
    var t = new CustomerTransactionsReader("C:\\tmp\\DMas2\\Groceries.csv")
//    var t = new UnixTransactionsReader("C:\\tmp\\DMas2\\UNIX_usage\\USER1\\sanitized_all.981115184025")

    var Minier = new FPtreeMining(t.transactions, 0.03 )
    var rules = generateRules(Minier.frequentItemsets, 0.2, t.transactions.length)
    for(each<- rules){
      each.printRule()
    }
  }
}

class Rule(headerA:Set[String], tailA:Set[String], supportA:Double, convinceA:Double) {
  val header:Set[String] = headerA
  val tail:Set[String] = tailA
  val support:Double = supportA 
  val convince:Double = convinceA
  def printRule(): Unit ={
    print("Rule: "+header.toString+" ==> "+tail.toString)
    printf("\t\t support: %f, convince: %f \n", support, convince)
  }
}

