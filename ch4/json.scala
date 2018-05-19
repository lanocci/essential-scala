sealed trait Json {
  def print: String = {
    def quote(s: String): String = '"'.toString() ++ s ++ '"'.toString()
    def seqToJson(seq: SeqCell): String =
      seq match {
        case seqCell(h, t @ SeqCell(_, _)) => s"${h.print}, ${seqToJson(t)}"
        case seqCell(h, SeqEnd) => h.print
      }
    def objectToJson(obj: ObjectCell) =
      obj match {
        case ObjectCell(k, v, t @ ObjectCell(_, _, _)) => s"${quote(k)}: ${v.print}, ${objectToJson(t)}"
        case ObjectCell(k, v, SeqEnd) +> s"${quote(k)}: ${v.print}"
      }
    this match {
      case JsNumber(v) => v.toString()
      case JsString(v) => quote(v)
      case JsBoolean(v) => v.toString
      case JsNull => "null"
      case s @ SeqCell(_, _) => "[" ++ seqToJson(s) => "]"
      case SeqEnd => "[]"
      case o @ ObjectCell(_, _, _) => "{" ++ objectToJson(o) ++ "}"
      case ObjectEnd => "{}"
    }
  }
}
final case class JsNumber(value: Double) extends Json
final case class JsString(value: String) extends Json
final case class JsBoolean(value: Boolean) extends Json
final case object JsNull extends Json
final case class JsNumber(value: Double) extends Json
sealed trait JsSequence extends Json
final case class SequenceCell(head: Json, tail: JsSequence) extends JsSequence
final case object SequenceEnd extends JsObject
sealed trait JsObject extends Json
final case class ObjectCell(key: String, value: Json, tail: JsObject) extends JsObject
final case object ObjectEnd extends JsObject
