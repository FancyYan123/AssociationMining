import scala.collection.mutable.{Map, Set}
import java.util

import sun.text.resources.pt.FormatData_pt
class FPtreeMining(transactionsA:List[Array[String]], minSupportA:Double) {
  //用字典存储所有的频繁项集，key为int，表示k频繁，value是所有的项集与其频率组成的map
  var frequentItemsets:Map[Int, Map[Set[String], Int]] = Map[Int, Map[Set[String], Int]]()

  var minSupport:Int = (minSupportA*transactionsA.length).toInt
  var transactions:List[Array[String]] = transactionsA

  var transactionsFreq = new Array[Int](transactions.length)
  for(i <- transactionsFreq.indices){
    transactionsFreq(i) = 1
  }
  var originTree = new FPtree(transactions, transactionsFreq, minSupport)
  FPgrowth(originTree, Set())


  def FPgrowth(tree:FPtree, prePattern:Set[String]): Unit ={
    var headersList = tree.headersList.reverse
    var headers = tree.headers.reverse

    for(each<-headers){
      if(each._2>=minSupport) {
        var pattern = prePattern + each._1
        if(frequentItemsets.contains(pattern.size))
          frequentItemsets(pattern.size)(pattern) = each._2
        else {
          frequentItemsets(pattern.size) = Map[Set[String], Int]()
          frequentItemsets(pattern.size)(pattern) = each._2
        }
      }
    }
    for(index<-headersList.indices){
      var patternBase = prePattern + headers(index)._1
      var tmpTransactions = List[Array[String]]()
      var tmpTransactionsFreq = List[Int]()

      var tmpList = headersList(index)
      for(eachNode<-tmpList){
        var transaction = List[String]()
        var freq = eachNode.freq
        var p = eachNode.parent match {case Some(node)=>node}
        while(p.name != "Null"){
          transaction = p.name::transaction
          p = p.parent match {case Some(node)=>node}
        }
        if(transaction.nonEmpty) {
          tmpTransactions = transaction.toArray :: tmpTransactions
          tmpTransactionsFreq = freq :: tmpTransactionsFreq
        }
      }
      if(tmpTransactions.nonEmpty) {
        var tmpTree = new FPtree(tmpTransactions, tmpTransactionsFreq.toArray, minSupport)
        if (!tmpTree.isEmpty())
          FPgrowth(tmpTree, patternBase)
      }
    }
  }
}

class FPtree (transactionsA:List[Array[String]],
              transactionsFreqA:Array[Int],
              minSupportA:Int) {
  var minSupport = minSupportA
  var transactions = transactionsA
  var transactionsFreq = transactionsFreqA

  var headers:Array[(String, Int)] = calcOneFreqItemsets()
  var (root, headersList) = constructFPtree(transactions, transactionsFreq, headers)

  def isEmpty():Boolean={
    return headers.length==0
  }
  //计算一频繁项集，根据支持度排序，使其成为header表
  def calcOneFreqItemsets():Array[(String, Int)] = {
    var freqOne = Map[String, Int]()
    var freqencyMap = Map[String, Int]()
    for(i<-transactions.indices){
      val transaction = transactions(i)
      val frequency = transactionsFreq(i)
      for(item<-transaction){
        if(freqencyMap.contains(item))
          freqencyMap(item) += frequency
        else
          freqencyMap(item) = frequency
      }
    }
    for(item<-freqencyMap){
      if(item._2>=minSupport)
        freqOne(item._1) = item._2
    }
    //从大到小排序
    freqOne.toArray.sortBy(-_._2)
  }

  //构造一颗FP树
  def constructFPtree(transactions:List[Array[String]],
                      transactionFreq:Array[Int],
                      headers: Array[(String, Int)]
                     ): (FPtreeNode, Array[List[FPtreeNode]]) = {
    var root:FPtreeNode = new FPtreeNode("Null", None)
    var headerLists:Array[List[FPtreeNode]] = new Array[List[FPtreeNode]](headers.length)
    if(headers.isEmpty)
      return (root, headerLists)

    for(i<- headers.indices)
      headerLists(i) = Nil

    var allFreqItems = Set[String]()
    for(each<-headers){
      allFreqItems.add(each._1)
    }

    for(i<-transactions.indices){
      val transaction = transactions(i)
      val freq = transactionFreq(i)
      var toInsertSet = Set[String]()
      for(each <- transaction)
        if(allFreqItems.contains(each))
          toInsertSet.add(each)

      var toInsertArray = new Array[(String,Int)](toInsertSet.size)
      var index = 0
      for(i <- headers.indices){
        if(toInsertSet.contains(headers(i)._1)) {
          toInsertArray(index) = (headers(i)._1, i)
          index += 1
        }
      }

      //insert into the tree:
      var p = root
      for((name, index) <- toInsertArray){
        var flag = true
        for(child<-p.children){
          if(child.name == name){
            child.freq += freq
            p = child
            flag = false
          }
        }
        if(flag){
          var node:FPtreeNode = new FPtreeNode(name, Some(p))
          node.freq = freq
          p.children = node::p.children
          headerLists(index) = headerLists(index).::(node)
          p = node
        }
      }
    }

    (root, headerLists)
  }

}

class FPtreeNode(nameA:String, parentA:Option[FPtreeNode]) {
  var name = nameA
  var freq = 0

  var parent:Option[FPtreeNode] = parentA match {
    case None => None
    case Some(p) => Some(p)
  }
  //var parent = parentA

  var children: List[FPtreeNode] = List[FPtreeNode]()

}

object FPmain{
  def main(args:Array[String]):Unit = {
//    var allTrans:List[Array[String]] = List(Array("I1", "I2", "I5"), Array("I2", "I4"), Array("I2", "I3"), Array("I1", "I2", "I4"),
//      Array("I1", "I3"), Array("I2", "I3"), Array("I1", "I3"), Array("I1", "I2", "I3", "I5"), Array("I1", "I2", "I3"))
    val minSupport = 0.01
    val startTime = System.currentTimeMillis()
    var t = new CustomerTransactionsReader("C:\\tmp\\DMas2\\Groceries.csv")
    var fpMinier = new FPtreeMining(t.transactions, minSupport)
    val endTime = System.currentTimeMillis()

    printf("support: %f, time cost: %f \n", minSupport, (endTime-startTime)/1000.0)
  }
}
