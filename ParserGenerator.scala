import scala.collection.mutable.ListBuffer
import java.io.PrintWriter

object ParserGenerator { /* "parser.scala"Çê∂ê¨ */
  val parser = new PrintWriter("Parser.scala")
  def parserGenerator(operations : ListBuffer[Operation], NTnum : Map[String, Int], tableList : ListBuffer[Array[Array[Option[Int]]]]) : Unit = {

    val strstart = """
import scala.io.Source
import scala.collection.mutable.ListBuffer
object Parser {
  var NTlist = new ListBuffer[String]
  def main(args : Array[String]) {
    if (args.length != 1) {
      println("Error: wrong number of arguments")
      return
    }
    val source = Source.fromFile(args(0))
    var string = source.mkString
    source.close
    val operations : Array[Operation] = Array("""
    
    parser.print(strstart)
    var firstFlg = true
    var i = 0
    for (operation <- operations) {
      if (!firstFlg) {
        parser.print(", ")
      }
      else {
        firstFlg = false
      }
      parser.print(operation.toStr())
      i += 1
      if (i > 9) {
        parser.println()
        parser.print("                                            ")
        i = 0
      }
    }
    parser.print(")")

    val str2 = """
    NTlist = ListBuffer("""
    parser.print(str2)
    firstFlg = true
    for ((nt, _) <- NTnum) {
      if (!firstFlg) {
        parser.print(", ")
      }
      else {
        firstFlg = false
      }
      parser.print("\"")
      parser.print(nt)
      parser.print("\"")
    }
    parser.print(")")
    
    val str3 = """
    val tables = Array[Array[Array[Option[Int]]]]("""
    parser.print(str3)
    var first1 = true
    var first2 = true
    var first3 = true
    for (table <- tableList) {
      if (!first1) {
        parser.print(", ")
        parser.println()
        parser.print("                                                  ")
      }
      else {
        first1 = false
      }
      parser.print("Array[Array[Option[Int]]](")
      for (x <- table) {
        if (!first2) {
          parser.print(", ")
          parser.println()
          parser.print("                                                                            ")
        }
        else {
          first2 = false
        }
        parser.print("Array[Option[Int]](")
        for (y <- x) {
          if (!first3) {
            parser.print(", ")
          }
          else {
            first3 = false
          }
          parser.print(y)
        }
        first3 = true
        parser.print(")")
        
      }
      first2 = true
      parser.print(")")
    }
    parser.print(")")
    
    val strend = """
    val startTime : Long = System.currentTimeMillis()
    val result = VirtualMachine.virtualMachine(operations, string, tables)
    println(string)
    println("Time = " + (System.currentTimeMillis() - startTime)+" msec")
    println("result = "+result._1)
    if (!result._2.isEmpty) {
      printSV(result._2.head)
    }
  }
  def getNT(n : Int) : String = {
    if (n < NTlist.size) {
      return(NTlist(n))
    }
    else {
      return("")
    }
  }
  def printSV(nt : Any) : Unit = {
    print("SV = ")
    pSV(nt, true)
    print("\n")
    def pSV(nt : Any, last : Boolean) : Unit = {
      nt match {
        case nt : ListBuffer[Any] => {
          if (nt.size == 1) {
            print("\""+nt.head+"\"")
          }
          else if (nt.size > 1) {
            print(nt.head)
            print(" (")
            for (i <- 1 until nt.size) {
              pSV(nt(i), (i >= nt.size - 1))
            }
            print(')')
          }
        }
        case _ => {
          print("\""+nt+"\"")
        }
      }
      if(!last) {
        print(", ")
      }
    }
  }
}
"""
      parser.println(strend)
      parser.close()
  }
}