import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

object Optimization {
  def RewPeg(str : String) : String = { /* PEG���������֐� */
      var result = "" // ������������
      var conj = ListBuffer[String]("") /* ���ʍ� */
      var ChoicedStr = ListBuffer[String]() /*�@'/'�ŋ�؂�ꂽstring�̃��X�g */
      var str2 = "" // <-���E��string
      breakable {
        for (j <- 0 until str.length - 2) { /* result��str2�ɕ��� */
          if (str(j) == '<' && str(j + 1) == '-') {
            result = str.substring(0, j + 2)
            str2 = str.substring(j + 2)
            break
          }
        }
      }
      var cnum = 0
      var conj2 = "" //���ʍ�
      var cfirst = true
      PegChoice(str2) /* conj, ChoicedStr�ɒl������ */
      //println("conj : "+conj)
      //println("ChoicedStr : "+ChoicedStr)
      breakable {
        for (c <- conj) { //���ʍ��̃��X�g����
          if (c == "") {
            break
          }
          result += c
          result += '_'
          cnum += 1
          if (cfirst) {
            cfirst = false
          }
          else {
            conj2 += '_'
          }
          conj2 += c
        }
      }
      if (cnum > 0) {
        result += '<'
        var first = true
        for (s <- ChoicedStr) { /* '/'�ŋ�؂�ꂽString�̃��X�g���� */
          var checkstr = s
          for (i <- 0 until cnum) { /* ���ʍ��̐������� */
            //println("checkstr : "+checkstr)
            breakable {
              for (j <- 0 until checkstr.length - 1) { /* '_'�ŋ�؂� */
                if (checkstr(j) == '_') {
                  checkstr = checkstr.substring(j + 1)
                  break
                }
              }
            }
          }
          if (first) {
            first = false
            result += checkstr
          }
          else if (checkstr == conj2) {
            result += '/'
            result += '��'
          }
          else {
            result += '/'
            result += checkstr
          }
        }
        result += '>'
      }
      else {
        result = str
      }
      
      def PegChoice(str : String) : Unit = {
        //println("choice : "+str)
        if (str.length == 0) {
          return
        }
        var st1 = ""
        var st2 = ""
        breakable {
          for (j <- 0 until str.length - 1) {
            if (str(j) == '/') {
              st1 = str.substring(0, j)
              ChoicedStr += st1
              st2 = str.substring(j + 1)
              break
            }
          }
        }
        if (st1 == "" || st2 == "") { /* '/'�����݂��Ȃ� */
          ChoicedStr += str
          PegSeq(str, 0)
        }
        else { // PegSeq'/'PegChoice
          PegSeq(st1, 0)
          PegChoice(st2)
        }
      }
      def PegSeq(str : String, lnum : Int) : Unit = {
        //println("seq : "+str)
        //println(conj, lnum)
        var st1 = ""
        var st2 = ""
        breakable {
          for (j <- 0 until str.length - 1) {
            if (str(j) == '_') {
              st1 = str.substring(0, j)
              st2 = str.substring(j + 1)
              break
            }
          }
        }
        if (st1 == "" || st2 == "") { /* '_'�����݂��Ȃ� */
          if (conj.length == lnum) { /* conj(lnum)���� */
            conj += str
          }
          else if (conj(lnum) != str) { /* conj(lnum)���ʂ̕����� */
            conj(lnum) = ""
          }
          else if (conj.length > lnum + 1) { /* conj(cnum)������������Ń��X�g�̎������݂��� */
            conj(lnum + 1) = ""
            //conj(lnum) = "" /* conj(cnum)������������ł��A�Ō�̕������Ɩ��Ӗ� */
          }
          else {
            //conj(lnum) = "" /* conj(cnum)������������ł��A�Ō�̕������Ɩ��Ӗ� */
          }
        }
        else { // '_'������
          if (conj.length == lnum) { /* conj(lnum)���� */
            conj += st1
            PegSeq(st2, lnum + 1)
          }
          else if (conj(lnum) == "") { /* conj(lnum)���� */
            conj(lnum) = st1
            PegSeq(st2, lnum + 1)
          }
          else if (conj(lnum) == st1) { /* conj(lnum)������������ */
          PegSeq(st2, lnum + 1)
          }
          else { /* conj(cnum)���ʂ̕����� */
            conj(lnum) = ""
          }
        }
      }
      return result
    }
}