abstract class PegTerm
	case class TmEmpty() extends PegTerm
	case class TmChar(c : Char) extends PegTerm
	case class TmRepChar(c : Char) extends PegTerm
	case class TmChoicedChar(c : Char, p : PegTerm) extends PegTerm
	case class TmAnyChar() extends PegTerm
	case class TmString(str : String) extends PegTerm
	case class TmRepString(str : String) extends PegTerm
	case class TmChoicedString(str : String, p : PegTerm) extends PegTerm
	case class TmCharSet(set : String) extends PegTerm
	case class TmChoicedCharSet(set : String, p : PegTerm) extends PegTerm
	case class TmNotPred(peg : PegTerm) extends PegTerm
	case class TmAndPred(peg : PegTerm) extends PegTerm
	case class TmConnect(peg1 : PegTerm, peg2 : PegTerm) extends PegTerm
	case class TmChoice(peg1 : PegTerm, peg2 : PegTerm) extends PegTerm
	case class TmRepete(peg1 : PegTerm) extends PegTerm
	case class TmRepeteOne() extends PegTerm
	case class TmOneOrNone() extends PegTerm
	case class TmVariable(label : String) extends PegTerm
	case class TmClosedGrammar(g : Map[String, PegTerm], label : String) extends PegTerm
	case class TmVariableWithPG(label : String, pg : String) extends PegTerm
	case class TmGrammar(label : String) extends PegTerm
	case class TmAutomaton(num : Int) extends PegTerm
	case class TmTableJmp(peg : PegTerm) extends PegTerm