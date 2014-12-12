import scala.collection.mutable.ListBuffer
abstract class Operation { /* ‰¼‘zƒ}ƒVƒ“–½—ß */
  def toStr() : String = {
    return this.toString()
  }
}
case class OpChar(c : Char) extends Operation {
  override def toStr() : String = {
    return "OpChar('"+c+"')"
  }
}
case class OpChoicedChar(c : Char, l : Int) extends Operation {
  override def toStr() : String = {
    return "OpChoicedChar('"+c+"', "+l+")"
  }
}
case class OpRepChar(c : Char) extends Operation {
  override def toStr() : String = {
    return "OpRepChar('"+c+"')"
  }
}
case class OpString(str : String) extends Operation {
  override def toStr() : String = {
    return "OpString(\""+str+"\")"
  }
}
case class OpChoicedString(str : String, l : Int) extends Operation {
  override def toStr() : String = {
    return "OpChoicedString(\""+str+"\", "+l+")"
  }
}
case class OpRepString(str : String) extends Operation {
  override def toStr() : String = {
    return "OpRepString(\""+str+"\")"
  }
}
case class OpAny() extends Operation
case class OpChoice(l : Int) extends Operation
case class OpJump(l : Int) extends Operation
case class OpCall(l : Int, nt : Int) extends Operation
case class OpCallReplace(n : String) extends Operation {
  override def toStr() : String = {
    return "OpCallReplace(\""+n+"\")"
  }
}
case class OpReturn() extends Operation
case class OpCommit(l : Int) extends Operation
case class OpFail() extends Operation
case class OpPartialCommit(l : Int) extends Operation
case class OpFailTwice() extends Operation
case class OpBackCommit(l : Int) extends Operation
case class OpTestChar(c : Char, l : Int) extends Operation
case class OpTestAny(n : Int, l : Int) extends Operation
case class OpCharSet(pt : String) extends Operation {
  override def toStr() : String = {
    return "OpCharSet(\""+pt+"\")"
  }
}
case class OpChoicedCharSet(pt : String, l : Int) extends Operation {
  override def toStr() : String = {
    return "OpChoicedCharSet(\""+pt+"\", "+l+")"
  }
}
case class OpEmpty() extends Operation
case class OpPushSp() extends Operation
case class OpReduce() extends Operation
case class OpConnectReduce() extends Operation
case class OpRemoveSemanticEntry() extends Operation
case class OpEnd() extends Operation
case class OpAutomaton(n : Int) extends Operation
case class OpTableJmp(c : Array[Char], n : Array[Int]) extends Operation {
  override def toStr() : String = {
    var str = "OpTableJmp(Array[Char]("
    var first = true
    for (char <- c) {
      if (!first) {
        str += ", "
      }
      str += "'" + char + "'"
      first = false
    }
    first = true
    str += "), Array[Int]("
    for (int <- n) {
      if (!first) {
        str += ", "
      }
      str += int
      first = false
    }
    return str+"))"
  }
}