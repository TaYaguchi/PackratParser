import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

object VirtualMachine { /* ‰¼‘zƒ}ƒVƒ“ */
	val nil = -1
	val stack = new scala.collection.mutable.Stack[StackEntry]
	var stackFrame : ListBuffer[Any] = new ListBuffer[Any]
	var sp = 0 /* stack pointer */
	var pc = 0 /* program counter */
	var ip = 0 /* input data pointer */
	var endFlg = false
	var memo : Memo = null
	var btcount = 0
	var opcount = 0
	var tables = Array[Array[Array[Option[Int]]]]()

	def virtualMachine(program : Array[Operation], subject : String, tbls : Array[Array[Array[Option[Int]]]]) : (Boolean, ListBuffer[Any]) = {
		memo = new Memo()
		tables = tbls
		//println(program)
		while (!endFlg) {
		  val operation = program(pc)
		  if (pc >= program.length || operation == null) {
		    return (false, stackFrame)
		  }
		  operation match {
		    case OpEnd() => {
		      //println("input data pointer = "+ip)
		      //println("btcount : "+btcount)
		      //println("opcount : "+opcount)
		      //println("memocount : "+memo.memonum)
		      if (ip >= subject.length) {
		        return (true, stackFrame)
		      }
		      return (false, stackFrame)
		    }
			case _ => {
				execute(operation, subject)
			} 
		  }
		}
		return (false, stackFrame)
	}

	def pushStackFrame(sv : Any) : Unit = {
	  stackFrame += sv
		sp += 1
	}

	def popStackEntryAsList(n : Int) : ListBuffer[Any] = {
	    var entries = ListBuffer[Any]()
		while(stackFrame.length > n){
		 entries += stackFrame.remove(stackFrame.length - 1)
			sp -= 1
		}
		return entries.reverse
	}
	
	def execute(t : Operation, s : String) : Unit = {
	  opcount += 1
	  //println("operation = "+t+", pc = "+pc+", ip = "+ip)
	  //println(stack)
	  t match {
	    case OpChar(c) => {
	      if (ip >= s.length) {
	        execute(OpFail(), s)
	        }
	      else if (c == s(ip)) {
	        pc += 1
			ip += 1
			pushStackFrame(c)
	      }
	      else {
	        execute(OpFail(), s)
	        }
	      }
	    case OpChoicedChar(c, l) => {
	      if (ip >= s.length) {
	        execute(OpFail(), s)
	        }
	      else if (c == s(ip)) {
	        pc += l
	        ip += 1
	        pushStackFrame(c)
	        }
	      else {
	        pc += 1
	        }
	    }
	    case OpRepChar(c) => {
	      if (ip >= s.length) {
	        pc += 1
	        }
	      else if (c == s(ip)) {
	        ip += 1
	        pushStackFrame(c)
	        }
	      else {
	        pc += 1
	        }
	      }
		case OpCharSet(set) => {
		  var i = ip
		  var k = 0
		  var endflg = false
		  while(i < s.length && k < set.length && !endflg) {
		    if (k + 1 < set.length && set(k + 1) == '-'){
		      if (set(k) <= s(i) && s(i) <= set(k + 2)) {
		        pc += 1
		        ip += 1
				endflg = true
			  } 
		      else {
		        k = k + 3
		      }
		    }
		    else{
		      if (set(k) == '\\' && k + 1 < set.length && set(k + 1) == '-'){
		        if (s(i) == '-'){
		          pc += 1
		          ip += 1
		          endflg = true	
		        }
		        else {
		          k = k + 2
		        }
		      } 
		      else if (set(k) == s(i)) {
		        pc += 1
		        ip += 1
		        endflg = true
		        } 
		      else {
		        k = k + 1
		      }
		    }
		  }
		  if (!endflg) {
		    execute(OpFail(), s)
		  } 
		  else {
		    pushStackFrame(s(i))
		    }
		  }
		case OpChoicedCharSet(set, l) => {
		  var i = ip
		  var k = 0
		  var endflg = false
		  while(i < s.length && k < set.length && !endflg) {
		    if (k + 1 < set.length && set(k + 1) == '-'){
		      if (set(k) <= s(i) && s(i) <= set(k + 2)) {
		        pc += l
		        ip += 1
		        endflg = true
		      } 
		      else {
		        k = k + 3
		      }
		    }
		    else{
		      if (set(k) == '\\' && k + 1 < set.length && set(k + 1) == '-'){
		        if (s(i) == '-'){
		          pc += l
		          ip += 1
		          endflg = true	
		        } 
		        else {
		          k = k + 2
		        }
		      } 
		      else if (set(k) == s(i)) {
		        pc += l
		        ip += 1
		        endflg = true
		      } 
		      else {
		        k = k + 1
		      }
		    }
		  }
		  if (!endflg) {
		    pc += 1
		  } 
		  else {
		    pushStackFrame(s(i))
		  }
		}
		case OpString(str) => {
			if (ip >= s.length) {
			  execute(OpFail(), s)
			}
			else if (s.slice(ip, ip + str.length()) == str){
			  pc += 1
			  ip += str.length()
			  pushStackFrame(str)
			} 
			else {
			  execute(OpFail(), s)
			}
		}
		case OpAutomaton(n) => {
		  val str = s.slice(ip, s.length())
		  var pos = 0
		  var state : Option[Int] = Some(0)
		  val table = tables(n)
		  while(true) {
		    state = table(state.get)(str(pos).toInt - 'a'.toInt)
		    pos += 1
		    if(state.get == -1) { // success
		      pc += 1
		      pushStackFrame(s.slice(ip, ip + pos))
		      ip += pos
		      return
		    }
		    else if(state == None || pos > str.length - 1) { // failure
		      execute(OpFail(), s)
			  return
		    }
		  }
		}
		case OpTableJmp(cArray, iArray) => {
		  if (ip >= s.length) {
			  execute(OpFail(), s)
		  }
		  var endflg = false
		  var i = 0
		  breakable{
		    for (c <- cArray) {
		      if (c == s(ip)) {
		        pushStackFrame(c)
		        ip += 1
		        if (i == 0) {
		          pc += 1
		        }
		        else {
		          pc += iArray(i - 1)
		        }
		        endflg = true
		        break
		      }
		      i += 1
		    }
		  }
		  if (!endflg) {
		    execute(OpFail(), s)
		  }
		}
		case OpChoicedString(str, l) => {
		  if (ip >= s.length) {
		    execute(OpFail(), s)
		    }
			else if (s.slice(ip, ip + str.length()) == str){
			  pc += l
			  ip += str.length()
			  pushStackFrame(str)
			  }
			else {
			  pc += 1
			}
		}
		case OpRepString(str) => {
		  if (ip >= s.length) {
		    pc += 1
		  }
		  else if (s.slice(ip, ip + str.length()) == str){
		    ip += str.length()
		    pushStackFrame(str)
		    } 
		  else {
		    pc += 1
		  }
		}
		case OpAny() => {
		  if (s.length >= ip + 1) {
		    pushStackFrame(s(ip))
		    pc += 1
		    ip += 1
		    //println("OpAnyInput:" + s(ip - 1))
		    } 
		  else {
		    execute(OpFail(), s)
		    }
		  }
		case OpChoice(l) => {
		  stack.push(BackEntry(pc + l, ip, sp))
		  pc += 1
		  }
		case OpJump(l) => {
		  pc += l
		}
		case OpCall(l, nt) => {
		  val memoValue = memo.getMemo(nt, ip)
		  //println(memoValue);
		  if (memoValue._2 == None) { /* ‰½‚àƒƒ‚‚³‚ê‚Ä‚¢‚È‚¢ */
		    //println("no memo")
		    stack.push(ReturnEntry(pc + 1, sp, ip, nt))
		    pc += l
		  } 
		  else if (memoValue._2.get == -1) { //Ž¸”s‚ªƒƒ‚‚³‚ê‚Ä‚¢‚é
		    //println("memo : fail")
		    execute(OpFail(), s)
		    } 
		  else { /* ¬Œ÷‚ªƒƒ‚‚³‚ê‚Ä‚¢‚é */
		    //println("memo : success")
		    ip = memoValue._2.get
		    pc += 1
		    pushStackFrame(memoValue._1)
		    }
		  }
		case OpReturn() => {
		  val top = stack.pop.asInstanceOf[ReturnEntry]
		  pc = top.pc
		  val nt = Parser.getNT(top.nt)
		  var result : ListBuffer[Any] = ListBuffer(nt)
		  val values : ListBuffer[Any] = popStackEntryAsList(top.sp)
		  //println("nt = "+nt+", value = "+values)
		  result ++= values
		  memo.saveMemo(top.nt, top.ip, result, ip)
		  pushStackFrame(result)
		  }
		case OpCommit(l) => {
		  //stack.pop
		  stack.pop
		  pc += l
		  }
		case OpFail() => {
		  if (stack.isEmpty) {
		    endFlg = true
		    println("Stack is empty.")
		    return
		  }
		  if (ip >= s.length) {
		    ip = s.length - 1
		  }
		  val top = stack.pop
		  //println(top)
		  top match {
		    case BackEntry(pCount, pos, sv) => {
		      btcount += 1
		      popStackEntryAsList(sv)
		      pc = pCount
		      ip = pos
		    }
		    case ReturnEntry(pc, sp, ip, nt) => {
		      popStackEntryAsList(sp)
		      memo.saveMemo(nt, ip, 0, -1)
		      execute(OpFail(), s)
		    }
		    case StackPointerEntry(esp, eip) => {
		      popStackEntryAsList(esp)
		      execute(OpFail(), s)
		    }
		  }
		}
		case OpPartialCommit(l) => {
		  val top = stack.pop.asInstanceOf[BackEntry]
		  val sv = popStackEntryAsList(top.sp)
		  pushStackFrame(sv)
		  stack.push(BackEntry(top.pc, ip, sp))
		  pc += l
		  }
		case OpFailTwice() => {
		  val spe = stack.pop.asInstanceOf[StackPointerEntry]
		  val top = stack.pop.asInstanceOf[BackEntry]
		  execute(OpFail(), s)
		}
		case OpBackCommit(l) => {
		  val top = stack.pop.asInstanceOf[BackEntry]
		  pc += l
		  ip = top.pos
		}
		case OpTestChar(c, l) => {
		  if (c == s(ip)) {
		    pc += 1
		    ip += 1
		  } 
		  else {
		    pc += l
		  }
		}
		case OpTestAny(n, l) => {
		  if (ip + n <= s.length) {
		    pc += 1
		    ip += n
		    } 
		  else {
		    pc += l
		  }
		}
		case OpPushSp() => {
		  stack.push(StackPointerEntry(sp, ip))
		  //println(sp)
		  pc += 1
		}
		case OpConnectReduce() => {
		  val spe = stack.pop.asInstanceOf[StackPointerEntry]
		  val values = popStackEntryAsList(spe.sp)
		  pushStackFrame(values)
		  pc += 1
		}
		case OpRemoveSemanticEntry() => {
		  val top = stack.pop
		  //println(top)
		  pc += 1
		}
		case OpEmpty() => {
		  pc += 1
		}
	  }
	}
}