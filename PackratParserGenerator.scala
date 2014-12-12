import scala.io.Source

object PackratParserGenerator { /* PEG�K�����󂯎��C�p�[�U�𐶐�  */
  def main(args : Array[String]) : Unit = {
    if (args.length != 1) {
      println("Error: wrong number of arguments")
      return
    }
	val source = Source.fromFile(args(0), "UTF-8")
	val peg = source.mkString
	source.close
    val pegterms = PegCompiler.pegPerser(peg) /* PEG�K����PegTerm�ɕϊ� */   
    val operations = PiFunction.piFunction(pegterms._1, pegterms._2, pegterms._3, pegterms._4) /* PegTerm�����z�}�V�����߂ɕϊ� */
    ParserGenerator.parserGenerator(operations, pegterms._4, PegCompiler.tableList) /* ���z�}�V�����߂�������parser.scala�𐶐� */
  }
}