abstract class StackEntry
	case class BackEntry(pc : Int, pos : Int, sp : Int) extends StackEntry
	case class ReturnEntry(pc : Int, sp : Int, ip : Int, nt : Integer) extends StackEntry
	//case class SemanticEntry(sv : Any) extends StackEntry
	case class StackPointerEntry(sp : Int, ip : Int) extends StackEntry
