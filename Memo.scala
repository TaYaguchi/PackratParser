class Memo {
  private var memo = Map[(Int, Int), (Any, Option[Int])]().withDefaultValue((null, None))
  var memonum = 0
  def saveMemo(nt : Int, pos : Int, sv : Any, offset : Int) = {
    this.memo += (nt, pos) -> (sv, Some(offset))
    memonum += 1
    //this.memo += (nt, pos) -> (sv, None)
  }
  def getMemo(nt : Int, pos : Int) : (Any, Option[Int]) = {
    this.memo(nt, pos)
  }
}