import java.util

import scala.collection.mutable.Set
import scala.collection.mutable.Map
//import scala.collection.mutable.List

class AprioriMining(transactionsA:List[Array[String]], minSupportA:Double) {
  var transactions = transactionsA
  val minSupport = minSupportA*transactions.length

  //用字典存储所有的频繁项集，key为int，表示k频繁，value是所有的项集与其频率组成的map
  var frequentItemsets:Map[Int, Map[Array[String], Int]] = Map[Int, Map[Array[String], Int]]()
  var frequentItemsetsHashvalue = Map[Int, Set[Int]]()

  apriori()

  //def getFreqentItemsets():Map[Int, Set[Array[String]]] = frequentItemsets
  def apriori(): Unit ={
    val freqOne = calcOneFreqItemsets()
    var freqOneHash = Set[Int]()
    for(each<-freqOne)
      freqOneHash += calcHashValue(each._1)
    frequentItemsets(1) = freqOne
    frequentItemsetsHashvalue(1) = freqOneHash

    var k:Int = 2
    while(frequentItemsets.contains(k-1)){
      var candicates = aprioriGen(frequentItemsets(k-1), frequentItemsetsHashvalue(k-1))
      var frequent = Map[Array[String], Int]()
      for(itemset<-candicates){
        val freq = getItemsetFrequency(itemset)
        if(freq>minSupport)
          frequent(itemset) = freq
      }
      if(frequent.nonEmpty) {
        frequentItemsets(k) = frequent
        var hashvalue = Set[Int]()
        for(each <- frequent)
          hashvalue += calcHashValue(each._1)
        frequentItemsetsHashvalue(k) = hashvalue
      }
      k += 1
    }
  }

  def getItemsetFrequency(itemset:Array[String]):Int = {
    var support = 0
    for(transaction<-this.transactions){
      var flag = true
      for(each<-itemset)
        if(!transaction.contains(each))
          flag = false
      if(flag) support += 1
    }
    support
  }

  def calcOneFreqItemsets():Map[Array[String], Int] = {
    var freqOne = Map[Array[String], Int]()
    var freqencyMap = Map[String, Int]()
    for(transaction<-transactions){
      for(item<-transaction){
        if(freqencyMap.contains(item))
          freqencyMap(item) += 1
        else
          freqencyMap(item) = 1
      }
    }
    for(item<-freqencyMap){
      if(item._2>minSupport)
        freqOne(Array(item._1)) = item._2
    }
    freqOne
  }

  def calcHashValue(itemset:Array[String]): Int = {
    var hashvalue = 0
    for(each <- itemset){
      hashvalue += each.hashCode
    }
    hashvalue
  }

  def calcHashValue(itemset:Array[String], index:Int): Int = {
    var hashvalue = 0
    var i = 0
    while (i<itemset.length){
      if(i!=index)
        hashvalue += itemset(i).hashCode
      i += 1
    }
    hashvalue
  }

  def hasInfrequentSubset(c:Array[String], L_hash:Set[Int]):Boolean = {
    var index = 0
    while(index < c.length){
      var hashvalue = calcHashValue(c, index)
      if(!L_hash.contains(hashvalue))
        return true
      index += 1
    }
    return false
  }

  def aprioriGen(L_pre:Map[Array[String], Int], L_hash:Set[Int]): Set[Array[String]] = {
    //从L(k-1)频繁项集中产生k频繁项集的候选集
    //note: L_pre中的每一个项集都是排序过的
    var (i, j) = (0, 0)
    var L = L_pre.keySet.toArray
    val (set_size, items_length) = (L.length, L(0).length)
    var candicates = Set[Array[String]]()
    for(i <- 0 until set_size){
      for(j <- i+1 until set_size){
        if(i!=j){
          var flag = true
          for(k <- 0 until items_length-1 if flag){
            if (L(i)(k) != L(j)(k)){
              flag = false
            }
          }
          if(flag){
            var candidateItems = new Array[String](items_length+1)
            for(index<-0 until items_length-1)
              candidateItems(index) = L(i)(index)
            var str1:String = L(i)(items_length-1)
            var str2:String = L(j)(items_length-1)
            candidateItems(items_length-1) = if(str1<str2) str1 else str2
            candidateItems(items_length) = if(str1<str2) str2 else str1
            if(!hasInfrequentSubset(candidateItems, L_hash))
              candicates += candidateItems
          }
        }
      }
    }
    candicates
  }


}



object AprioriMain {
  def main(args: Array[String]): Unit = {
    val minSupport = 0.05
    val startTime = System.currentTimeMillis()
    var allTransactions = new CustomerTransactionsReader("C:\\tmp\\DMas2\\Groceries.csv")
    var aprioriMiner = new AprioriMining(allTransactions.transactions, minSupport)
    val endTime = System.currentTimeMillis()

    printf("support: %f, time cost: %f \n", minSupport, (endTime-startTime)/1000.0)
  }
}