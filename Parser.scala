
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
    val operations : Array[Operation] = Array(OpCall(2,0), OpEnd(), OpTableJmp(Array[Char]('a', 'd'), Array[Int](6)), OpChoice(3), OpChar('b'), OpCommit(2), OpChar('c'), OpJump(2), OpChar('e'), OpReturn()
                                            )
    NTlist = ListBuffer("E1")
    val tables = Array[Array[Array[Option[Int]]]]()
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

