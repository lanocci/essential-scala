sealed trait CalcResult
final case class CalcSucceed(result: Int) extends CalcResult
final case class CalcFailed(reason: String) extends CalcResult
