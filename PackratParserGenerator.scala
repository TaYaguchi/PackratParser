import scala.io.Source

object PackratParserGenerator { /* PEG規則を受け取り，パーザを生成  */
  def main(args : Array[String]) : Unit = {
    if (args.length != 1) {
      println("Error: wrong number of arguments")
      return
    }
	val source = Source.fromFile(args(0), "UTF-8")
	val peg = source.mkString
	source.close
    val pegterms = PegCompiler.pegPerser(peg) /* PEG規則をPegTermに変換 */   
    val operations = PiFunction.piFunction(pegterms._1, pegterms._2, pegterms._3, pegterms._4) /* PegTermを仮想マシン命令に変換 */
    ParserGenerator.parserGenerator(operations, pegterms._4, PegCompiler.tableList) /* 仮想マシン命令を持ったparser.scalaを生成 */
  }
}