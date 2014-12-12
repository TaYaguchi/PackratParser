import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

object PegCompiler { 
  var tableList = ListBuffer[Array[Array[Option[Int]]]]() /* �I�[�g�}�g����table�̃��X�g */
  def pegPerser(peg : String) : (ListBuffer[PegTerm], (String, PegTerm), Map[String, PegTerm], Map[String, Int]) = { /* Peg�K�����󂯎��CPegTerm�̃��X�g��Ԃ� */
    val special = false /* ChoicedChar�Ȃǂ̎g�p */
    val automaton = false /* Automaton���߂̎g�p */
    val tableJmp = true /* tableJmp���߂̎g�p */
    var print = false /* �f�o�b�O�p */
    
    var pegList = ListBuffer[PegTerm]() /* peg�̃��X�g */
    var firstNT : (String, PegTerm) = ("", TmEmpty()) /* �ŏ��̔�I�[�L���ƑΉ�����PegTerm */
    var NTmap = Map[String, PegTerm]() /* �ŏ��ȊO�̔�I�[�L���ƑΉ�����PegTerm��MAP */
    var NTnum = Map[String, Int]() /* ��I�[�L�����i���o�����O  */
    var istr = ""
    var first = true
    
    for (s <- peg.lines) { /* PEG�K�����s���Ƃ�NT��PegTerm�ɑΉ��Â��� */
      istr = ""
      println("PEG : "+s)
      //val istr2 = Optimization.RewPeg(istr)
      //println("PEG' : "+istr2)
      val x : (Option[PegTerm], String, PegTerm) = Rule(s)
      if (x._1 == None) {
        println("parse missed.")
      }
      else { /* success */
        pegList += x._1.get
        NTnum += (x._2 -> NTnum.size)
        var NT : (String, PegTerm) = ("", TmEmpty())
        if (automaton) {
          NT = (x._2, OptTerm(RewPegTerm(x._3)))
        }
        else if (tableJmp) {
          var peg = RewPegTerm(x._3)
          if (IsCharSwitch(peg)) {
            peg = SwitchChar(peg)
            if (IsTableJmp(peg)) {
              NT = (x._2, TmTableJmp(peg))
            }
            else {
              NT = (x._2, peg)
            }
          }
          else {
            NT = (x._2, RewPegTerm(x._3))
          }
        }
        else {
          NT = (x._2, RewPegTerm(x._3))
        }
        if (first) {
          firstNT = NT
          first = false
        }
        else {
          NTmap += NT
        }
      }
    }
    println(firstNT._1+" : "+firstNT._2)
    for ((nt, pt) <- NTmap) {
      println(nt+" : "+pt)
    }
    
    def Rule(str : String) : (Option[PegTerm], String, PegTerm) =  { /* Peg��1�s�󂯎��CNT��PegTerm�CNT��String�C�Ή�����PegTerm��Ԃ� */
      if(print)
        println("Rule   : " + str)
      var st1 = "" /* NT */
      var st2 = "" /* NT�ɑΉ�����PEG */
      breakable {
        for (j <- 0 until str.length - 2) {
          if (str(j) == '<' && str(j + 1) == '-') {
            if (str(j - 1) == ' ') {
              st1 = str.substring(0, j - 1)
            }
            else {
              st1 = str.substring(0, j)
            }
            if (str(j + 2) == ' ') {
              st2 = str.substring(j + 3)
            }
            else {
              st2 = str.substring(j + 2)
            }
            break
          }
        }
      }
      if (st1 == "" || st2 == "")
        return (None, "", TmEmpty())
      val res1 = NT(st1)
      val res2 = PE(st2)
      res1.get match { /* ��I�[�L���̃��x�����擾���A�}�b�s���O���� */
        case TmVariable(label) => {
          //NTnum += label -> NTnum.size
          //NTmap += label -> res2.get
          return (res1, label, res2.get)
        }
        case _ => {return (None, "", TmEmpty())}
      }
    }
    def NT(str : String) : (Option[PegTerm]) = {
      if(print)
        println("NT     : " + str)
      if (str.length == 0) {
        return None
      }
      return Name(str)
    }
    def Name(str : String) : (Option[PegTerm]) = {
      var list = ('a' to 'z') ++ ('A' to 'Z')
      var check = false /* �A���t�@�x�b�g�������łȂ���false */
      for (n <- 0 until str.length) {
        breakable {
          for(alf <- list) {
            if (n == 0 && str(n) == alf) { /* �ꕶ���ڂ̓A���t�@�x�b�g���� */
              check = true
              break
            }
            else if(n != 0 && (str(n) == alf || str(n).isDigit)) {
              check = true
              break
            }
          }
        }
        if (!check && n == 0)
          return None
        if (!check)
          return (Some(TmVariable(str.substring(0, n))))
        check = false
      }
      return Some(TmVariable(str))
    }
    def PE(str : String) : (Option[PegTerm]) = {
      if(print)
        println("PE     : " + str)
      val res0 = Brac(str)
      lazy val res1 = Choice(str)
      lazy val res2 = Sequ(str)
      lazy val res3 = Term(str)
      lazy val res4 = NT(str)
      if (res0 != None)
        return res0
      else if (res1 != None) 
        return res1
      else if (res2 != None) 
        return res2
      else if  (res3 != None)
        return res3
      else if  (res4 != None)
        return res4
      else
        return None
    }
    def Brac(str : String) : (Option[PegTerm]) = {
      if(print)
        println("Brac   : " + str)
      if (str(0) != '(') {
        return None
      }
      var st1 = "" // (st1)st2
      var st2 = ""
      var bcount =  0 // ���ʂ̐�
      for (j <- 1 until str.length) {
        if (str(j) == '(') {
          bcount += 1
        }
        else if (str(j) == ')') {
          if (bcount > 0) {
            bcount -= 1
          }
          else {
            st1 = str.substring(1, j)
            if (st1 == "") {
              return None
            }
            var res1 = PE(st1)
            st2 = str.substring(j + 1)
            if (st2.length < 1) {
              return res1
            }
            st2(0) match {
              case '?' => {
                res1 = Some(TmChoice(res1.get, TmEmpty()))
                st2 = st2.substring(1)
              }
              case '+' => {
                res1 = Some(TmConnect(res1.get, TmRepete(res1.get)))
                st2 = st2.substring(1)
              }
              case '*' => {
                res1 = Some(TmRepete(res1.get))
                st2 = st2.substring(1)
              }
              case _ => {}
            }
            if (st2 == "") {
              return res1
            }
            if (st2.length > 3 && st2(0) == ' ' && st2(1) == '/') {
              val res2 = PE(st2.substring(3))
              return Some(TmChoice(res1.get, res2.get))
            }
            if (st2(0) == ' ') {
              val res2 = PE(st2.substring(1))
              return Some(TmConnect(res1.get, res2.get))
            }
            return None
          }
        }
      }
      return None
    }
    def Choice(str : String) : (Option[PegTerm]) = {
      if(print)
        println("Choice : " + str)
      var st1 = ""
      var st2 = ""
      var bcount = 0
      var bcount2 = 0
      breakable {
        for (j <- 0 until str.length - 2) {
          if (str(j) == '(')
            bcount += 1
          else if (str(j) ==')')
            bcount -= 1
          else if (str(j) == '[')
            bcount2 += 1
          else if (str(j) ==']')
            bcount2 -= 1
          else if (bcount == 0 && bcount2 == 0 && str(j) == '/' && str(j - 1) == ' ') {
            st1 = str.substring(0, j - 1)
            st2 = str.substring(j + 2)
            break
          }
        }
      }
      if (st1 == "" || st2 == "")
        return (None)
      val pe = PE(st2)
      lazy val res1 = Sequ(st1)
      lazy val res2 = Term(st1)
      lazy val res3 = NT(st1)
      if (pe == None)
        return None
      else if (res1 != None)
    	  return Some(TmChoice(res1.get, pe.get))
      else if (res2 != None) {
        if (special){
        	res2.get match {
        		case TmChar(c) => {
        			return Some(TmChoicedChar(c, pe.get))
        		}
        		case TmString(s) => {
        			return Some(TmChoicedString(s, pe.get))
        		}	
        		case TmCharSet(set) => {
        			return Some(TmChoicedCharSet(set, pe.get))
        		}	
        		case _ => {
        			return Some(TmChoice(res2.get, pe.get))
        		}
        	}
        }
        else {
          return Some(TmChoice(res2.get, pe.get))
        }
      }
      else if (res3 != None) {
    	 return Some(TmChoice(res3.get, pe.get))
      }
      else
        return None
    }
    def Sequ(str : String) : (Option[PegTerm]) = {
      if(print)
        println("Seq    : " + str)
      var st1 = ""
      var st2 = ""
      var  bcount = 0
      var bcount2 = 0
      breakable {
        for (j <- 0 until str.length - 1) {
          if (str(j) == '(')
            bcount += 1
          else if (str(j) ==')')
            bcount -= 1
          else if (str(j) == '[')
            bcount2 += 1
          else if (str(j) ==']')
            bcount2 -= 1
          else if (bcount == 0 && bcount2 == 0 && str(j) == ' ') {
            st1 = str.substring(0, j)
            st2 = str.substring(j + 1)
            break
          }
        }
      }
      if (st1 == "" || st2 == "")
        return None
      val pe = PE(st2)
      lazy val res1 = Term(st1)
      lazy val res2 = NT(st1)
      if (pe == None)
        return None
      else if (res1 != None)
        return Some(TmConnect(res1.get, pe.get))
      else if (res2 != None)
        return Some(TmConnect(res2.get, pe.get))
      else
        return None
    }
    def Term(str : String) : (Option[PegTerm]) = {
      if(print)
        println("Term   : " + str)
      if (str == ".") {
        return Some(TmAnyChar())
      }
      if (str.length == 0) {
        return None
      }
      else if (str(0) == '!') {
        val term = Term(str.substring(1))
        return Some(TmNotPred(term.get))
      }
      else if (str(str.length - 1) == '+') {
        val term = Term(str.substring(0, str.length - 1))
        if (special) {
        term.get match {
          case TmChar(c) => {
            return return Some(TmConnect(term.get, TmRepChar(c)))
          }
          case TmString(str) => {
            return Some(TmConnect(term.get, TmRepString(str)))
          }
          case _=> {
            return Some(TmConnect(term.get, TmRepete(term.get)))
          }
        }
        }
        else {
          return Some(TmConnect(term.get, TmRepete(term.get)))
        }
      }
      else if (str(str.length - 1) == '*') {
        val term = Term(str.substring(0, str.length - 1))
        if (special) {
        term.get match {
          case TmChar(c) => {
            return Some(TmRepChar(c))
          }
          case TmString(str) => {
            return Some(TmRepString(str))
          }
          case _=> {
            return Some(TmRepete(term.get))
          }
        }
        }
        else {
          return Some(TmRepete(term.get))
        }
      }
      else if (str(str.length - 1) == '?') {
        val term = Term(str.substring(0, str.length - 1))
        if (special) {
        term.get match {
          case TmChar(c) => {
            return Some(TmChoicedChar(c, TmEmpty()))
          }
          case TmString(str) => {
            return Some(TmChoicedString(str, TmEmpty()))
          }
          case _=> {
            return Some(TmChoice(term.get, TmEmpty()))
          }
          }
        }
        else {
            return Some(TmChoice(term.get, TmEmpty()))
          }
      }
      else if (str == "��") {
        return Some(TmEmpty())
      }
      str(0) match {
        case ''' | '"'=> { /* �N�H�[�e�[�V������_�u���N�H�[�e�[�V�����ň͂܂ꂽ�I�[�L��  */
          var str2 = ""
          breakable {
            for (j <- 1 until str.length) {
              if (str(j) == str(0)) {
                str2 = str.substring(1, j)
                break
              }
            }
          }
          
          if (str2.length < 2) {
            return Some(TmChar(str2(0)))
          }
          else {
            return Some(TmString(str2))
          }
        }
        case '[' => { /* �u���P�b�g�ň͂܂ꂽ�I�[�L�� */
          var str2 = ""
          breakable {
            for (j <- 1 until str.length) {
              if (str(j) == ']') {
                str2 = str.substring(1, j)
                break
              }
            }
          }
          return Some(TmCharSet(str2))
        }
        case _=> {
          return NT(str)
        }
      }
      
    }
    return (pegList, firstNT, NTmap, NTnum)
  }
  
  def RewPegTerm(peg : PegTerm) : PegTerm = { /* PegTerm���������ɏ������� */
    peg match {
      case TmChoice(p1, p2) => {
        p1 match {
          case TmChoice(pp1, pp2) => {
            return TmChoice(RewPegTerm(pp1), TmChoice(RewPegTerm(pp2), RewPegTerm(p2)))
          }
          case _ => {
            return TmChoice(RewPegTerm(p1), RewPegTerm(p2))
          }
        }
      }
      case _ => {
        return peg
      }
    }
  }
  
  	def IsCharSwitch(peg : PegTerm) : Boolean = { /* PegTerm���ŏ���1�����ŕ��򂷂�؂ł����true */
  	  peg match {
  	    case TmChoice(p1, p2) => {
  	      return (loop(p1) && loop(p2))
  	    }
  	    case _ => {
  	      return false
  	    }
  	  }
  	  def loop (peg : PegTerm) : Boolean = {
  	    peg match {
  	      case TmChar(_) => {
  	        return true
  	      }
  	      case TmChoice(p1, p2) => {
  	        return (loop(p1) && loop(p2))
  	      }
  	      case TmConnect(p1, _) => {
  	        p1 match {
  	          case TmChar(_) => {
  	            return true
  	          }
  	          case _ => return false
  	        }
  	      }
  	    }
  	    return false
  	  }
  	  return false
  	}
  	
  	def SwitchChar (peg : PegTerm) : PegTerm = { /* �ŏ��̕������ƂɃO���[�v�� */
  	  var charList = ListBuffer[(Char, ListBuffer[PegTerm])]() /* �ŏ��̕����ƑΉ�����PegTerm�̃��X�g */
  	  var result : PegTerm = TmEmpty()
  	  MakeCharList(peg)
  	  for ((c, list) <- charList.reverse) { /* charList����PegTerm���쐬 */
  	    var listpeg : PegTerm = TmEmpty()
  	    for (pegterm <- list.reverse) {
  	      if (list.length == 1) {
  	        if (pegterm == TmEmpty()){
  	          listpeg = TmChar(c)
  	        }
  	        else {
  	          listpeg = TmConnect(TmChar(c), pegterm)
  	        }
  	      }
  	      else {
  	        if (listpeg == TmEmpty()) {
  	          listpeg = pegterm
  	        }
  	        else {
  	          listpeg = TmChoice(pegterm, listpeg)
  	        }
  	      }
  	      
  	    }
  	    if (list.length > 1) {
  	      listpeg = TmConnect(TmChar(c), listpeg)
  	    }
  	    if (result == TmEmpty())
  	      result = listpeg
  	    else
  	      result = TmChoice(listpeg, result)
  	  }
  	  def MakeCharList (peg : PegTerm) { /* ���򂷂镶���ɑΉ�����PegTerm�̃��X�g�̃��X�g���쐬 */
  	    peg match {
  	      case TmChoice(p1, p2) => {
  	        p1 match {
  	          case TmChar(c) => {
  	            AddCharList(c, TmEmpty())
  	          }
  	          case TmConnect(pp1, pp2) => {
  	            pp1 match {
  	              case TmChar(c) => {
  	                AddCharList(c, pp2)
  	              }
  	            }
  	          }
  	        }
  	        MakeCharList(p2)
  	      }
  	      case TmChar(c) => {
  	        AddCharList(c, TmEmpty())
  	      }
  	      case TmConnect(p1, p2) => {
  	        p1 match {
  	          case TmChar(c) => {
  	            AddCharList(c, p2)
  	          }
  	        }
  	      }
  	    }
  	    def AddCharList (c : Char, peg : PegTerm) {
  	      var flg = false
  	      for ((char, list) <- charList) {
  	        if (c == char) {
  	          list += peg
  	          flg = true
  	        }
  	      }
  	      if (!flg)
  	        charList += ((c, ListBuffer[PegTerm](peg)))
  	    }
  	  }
  	  return result
  	}
  	
  	def IsTableJmp (peg : PegTerm) : Boolean = { /* table_jmp���߂ɕϊ����邩�̔��� */
  	  peg match {
  	    case TmChoice(_, _) => {
  	      return true
  	    }
  	  }
  	  return false
  	}
  
  def OptTerm (peg : PegTerm) : PegTerm = { /* TmAutomaton�ɕϊ��\�ȏꍇ�ϊ����C�e�[�u�����쐬 */
    peg match {
      case TmChoicedChar(c, p) => {
        return TmChoicedChar(c, OptTerm(p))
      }
      case TmNotPred(p) => {
        return TmNotPred(OptTerm(p))
      }
      case TmAndPred(p) => {
        return TmAndPred(OptTerm(p))
      }
      case TmConnect(p1, p2) => {
        return TmConnect(OptTerm(p1), OptTerm(p2))
      }
      case TmChoice(p1, p2) => {
        var pp = ConvAutomaton(peg, ListBuffer.empty[String], tableList.size)
        pp._1 match {
          case TmAutomaton(n) => {
            tableList += MakeTable(pp._2)
          }
          case _ => {}
        }
        return pp._1
      }
      case TmRepete(p) => {
        return TmRepete(OptTerm(p))
      }
      case TmString(s) => {
        tableList += MakeTable(ListBuffer[String](s))
        return TmAutomaton(tableList.size - 1)
      }
      case TmChoicedString(s, p) => {
        return TmChoicedString(s, OptTerm(p))
      }
      case TmChoicedCharSet(set, p) => {
        return TmChoicedCharSet(set, OptTerm(p))
      }
      return peg
    }
    def ConvAutomaton(peg : PegTerm, list : ListBuffer[String], autoNum : Int) : (PegTerm, ListBuffer[String]) = {
      var strList = list
      peg match {
        case TmChoice(p1, p2) => {
          p1 match {
            case TmString(s) => {
              p2 match {
                case TmChoice(pp1, pp2) => {
                  val conv = ConvAutomaton(p2, strList, autoNum)
                  conv._1 match {
                    case TmAutomaton(n) => {
                      s +=: strList
                      return (TmAutomaton(n), strList)
                    }
                    case _ => {
                      return (peg, ListBuffer.empty[String])
                    }
                  }
                }
                case TmString(ss) => {
                  strList += s
                  strList += ss
                  return (TmAutomaton(autoNum), strList)
                }
                case _ => {
                  return (peg, ListBuffer.empty[String])
                }
              }
            }
            case _ => {
              return (peg, ListBuffer.empty[String])
            }
          }
        }
      }
    }
    def MakeTable(words : ListBuffer[String]) : Array[Array[Option[Int]]] = {
    	var len = 0
    	for (word <- words) {
    		len += word.length - 1
    	}
    	var table : Array[Array[Option[Int]]] = Array.fill(len + 1, 26)(None)
    	var state = 0
    	var n = 0
    	for (word <- words) {
    		state = 0
    		for (i <- 0 until word.length) {
    			if (i == word.length - 1) { //success
    				table(state)(word(i).toInt - 'a'.toInt) = Some(-1)
    			}
    			else if (table(state)(word(i).toInt - 'a'.toInt) == None) {
    				n = n + 1
    				table(state)(word(i).toInt - 'a'.toInt) = Some(n)
    				state = n
    			}
    			else {
    				state = table(state)(word(i).toInt - 'a'.toInt).get
    			}
    		}
    	}
    	return table
    } 
    return peg
  }
}