import scala.collection.mutable.ListBuffer
object PiFunction { /* PegTerm‚ð‰¼‘zƒ}ƒVƒ“–½—ß—ñ‚É•ÏŠ· */
  var SV = false
  var firstTableJmp = true
  var tableJmpListChar = ListBuffer[Char]()
  var tableJmpListInt = ListBuffer[Int]()
  def piFunction(pegterms : ListBuffer[PegTerm], firstNT : (String, PegTerm), NTmap : Map[String, PegTerm], NTnum : Map[String, Int]) : ListBuffer[Operation] = {
    var program = ListBuffer[Operation]()
	var patchList = Map[String, Integer]()
	program += OpCallReplace(firstNT._1)
	program += OpEnd()
	patchList += (firstNT._1 -> program.length)
	program ++= convert(firstNT._2)
	program += OpReturn()
	for ((nt, pegterm) <- NTmap) {
	  patchList += (nt -> program.length)
	  program ++= convert(pegterm)
	  program += OpReturn()
	}
	val programList = backPatch(program, patchList, ListBuffer[Operation](), 1, NTnum)
	println(programList)
	return programList
  }
  def convert(peg : PegTerm) : ListBuffer[Operation] = {
    var operations = ListBuffer[Operation]()
    peg match {
      case TmChar(c) => {
        operations += OpChar(c)
      }
      case TmChoicedChar(c, p) => {
        val prog = convert(p)
        operations += OpChoicedChar(c, prog.length + 1)
        operations ++= prog
      }
      case TmRepChar(c) => {
        operations += OpRepChar(c)
      }
      case TmAnyChar() => {
        operations += OpAny()
      }
      case TmNotPred(p) => {
        val prog = convert(p)
        operations += OpChoice(prog.length + 3)
        if (SV) operations += OpPushSp()
        operations ++= prog
        operations += OpFailTwice()
      }
      case TmAndPred(p) => {
        operations ++= convert(TmNotPred(TmNotPred(p)))
      }
      case TmConnect(p1, p2) => {
        if (SV) operations += OpPushSp()
        operations ++= convert(p1)
        operations ++= convert(p2)
        if (SV) operations += OpRemoveSemanticEntry()
        //operations += OpConnectReduce()
      }
      case TmChoice(p1, p2) => {
        //println("TmChoice")
		val prog1 = convert(p1)
		val prog2 = convert(p2)
		operations += OpChoice(prog1.length + 2)
		//operations += OpPushSp()
		operations ++= prog1
		operations += OpCommit(prog2.length + 1)
		//operations += OpPushSp()
		operations ++= prog2
		//operations += OpRemoveSemanticEntry()
	  }
      case TmRepete(p) => {
        val prog = convert(p)
		operations += OpChoice(prog.length + 2)
		operations ++= prog
		operations += OpPartialCommit(-prog.length)
      }
      case TmEmpty() => {
        operations += OpEmpty()
      }
      case TmString(s) => {
        operations += OpString(s)
        
      }
      case TmChoicedString(s, p) => {
        val prog = convert(p)
        operations += OpChoicedString(s, prog.length + 1)
        operations ++= prog
      }
      case TmRepString(s) => {
        operations += OpRepString(s)
      }
      case TmCharSet(s) => {
        operations += OpCharSet(s)
      }
      case TmChoicedCharSet(set, p) => {
        val prog = convert(p)
        operations += OpChoicedCharSet(set, prog.length + 1)
        operations ++= prog
      }
      case TmVariable(label) => {
        operations += OpCallReplace(label)
      }
      case TmClosedGrammar(gg, label) => {
        operations ++= convert(gg(label))
      }
      case TmAutomaton(n) => {
        operations += OpAutomaton(n)
      }
      case TmTableJmp(p) => {
    	var first = false
        if (firstTableJmp) {
          first = true
          firstTableJmp = false
        }
        p match {
          case TmChoice(p1, p2) => {
            var len = 0
            p2 match {
              case TmChoice(_, _) => {
                val prog = convert(TmTableJmp(p2))
                len = prog.length
                prog ++=: operations
                OpJump(prog.length + 1) +=: operations
              }
              case TmChar(c) => {
                tableJmpListChar += c
              }
              case TmConnect(pp1, pp2) => {
                pp1 match {
                  case TmChar(c) => {
                    val prog = convert(pp2)
                    tableJmpListChar += c
                    prog ++=: operations
                    OpJump(prog.length + 1) +=: operations
                  }
                }
              }
            }
            p1 match {
              case TmChar(c) => {
                tableJmpListChar += c
                tableJmpListInt += 0
              }
              case TmConnect(pp1, pp2) => {
                pp1 match {
                  case TmChar(c) => {
                    val prog = convert(pp2)
                    tableJmpListChar += c
                    tableJmpListInt += prog.length + 1
                    prog ++=: operations
                  }
                }
              }
            }
            
            if (first) {
              var list = ListBuffer[Int]()
              for (num <- tableJmpListInt.reverse) {
                var sum = 0
                for (l <- list) {
                  sum += l
                }
                list += sum + num + 1
              }
              OpTableJmp(tableJmpListChar.reverse.toArray[Char], list.toArray[Int]) +=: operations
            }
          }
          case TmChar(c) => {
            tableJmpListChar += c
            operations += OpAny()
          }
          case TmConnect(pp1, pp2) => {
            pp1 match {
              case TmChar(c) => {
                val prog = convert(pp2)
                tableJmpListChar += c
                operations ++= prog
              }
            }
          }
        }
      }
    }
    return operations
  }
  def backPatch(list : ListBuffer[Operation], patchList : Map[String, Integer], fores : ListBuffer[Operation], pc : Integer, NTnum : Map[String, Int]) : ListBuffer[Operation] = {
    if (list.isEmpty) {
      return fores
      }
    list.head match {
      case OpCallReplace(label) => {
        return backPatch(list.tail, patchList, fores += OpCall(patchList(label) - pc + 1, NTnum(label)), pc + 1, NTnum)
      }
      case _ => {
        return backPatch(list.tail, patchList, fores += list.head, pc + 1, NTnum)
      }
    }
  }
}