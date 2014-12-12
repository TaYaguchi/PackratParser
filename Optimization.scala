import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

object Optimization {
  def RewPeg(str : String) : String = { /* PEG書き換え関数 */
      var result = "" // 書き換え結果
      var conj = ListBuffer[String]("") /* 共通項 */
      var ChoicedStr = ListBuffer[String]() /*　'/'で区切られたstringのリスト */
      var str2 = "" // <-より右のstring
      breakable {
        for (j <- 0 until str.length - 2) { /* resultとstr2に分割 */
          if (str(j) == '<' && str(j + 1) == '-') {
            result = str.substring(0, j + 2)
            str2 = str.substring(j + 2)
            break
          }
        }
      }
      var cnum = 0
      var conj2 = "" //共通項
      var cfirst = true
      PegChoice(str2) /* conj, ChoicedStrに値を入れる */
      //println("conj : "+conj)
      //println("ChoicedStr : "+ChoicedStr)
      breakable {
        for (c <- conj) { //共通項のリストを回す
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
        for (s <- ChoicedStr) { /* '/'で区切られたStringのリストを回す */
          var checkstr = s
          for (i <- 0 until cnum) { /* 共通項の数だけ回す */
            //println("checkstr : "+checkstr)
            breakable {
              for (j <- 0 until checkstr.length - 1) { /* '_'で区切る */
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
            result += 'ε'
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
        if (st1 == "" || st2 == "") { /* '/'が存在しない */
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
        if (st1 == "" || st2 == "") { /* '_'が存在しない */
          if (conj.length == lnum) { /* conj(lnum)が空 */
            conj += str
          }
          else if (conj(lnum) != str) { /* conj(lnum)が別の文字列 */
            conj(lnum) = ""
          }
          else if (conj.length > lnum + 1) { /* conj(cnum)が同じ文字列でリストの次が存在する */
            conj(lnum + 1) = ""
            //conj(lnum) = "" /* conj(cnum)が同じ文字列でも、最後の文字だと無意味 */
          }
          else {
            //conj(lnum) = "" /* conj(cnum)が同じ文字列でも、最後の文字だと無意味 */
          }
        }
        else { // '_'が存在
          if (conj.length == lnum) { /* conj(lnum)が空 */
            conj += st1
            PegSeq(st2, lnum + 1)
          }
          else if (conj(lnum) == "") { /* conj(lnum)が空 */
            conj(lnum) = st1
            PegSeq(st2, lnum + 1)
          }
          else if (conj(lnum) == st1) { /* conj(lnum)が同じ文字列 */
          PegSeq(st2, lnum + 1)
          }
          else { /* conj(cnum)が別の文字列 */
            conj(lnum) = ""
          }
        }
      }
      return result
    }
}