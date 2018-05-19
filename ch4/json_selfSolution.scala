/* Design Expresssion in Backus-Naur Form
 * JsonItem ::= JsNumber value:Double
 *            | JsString value:String
 *            | JsBoolean value:Boolean
 *            | JsNull
 *            | JsSequence
 *            | JsObject
 * JsSequence ::= SeqCell head:JsonItem tail:JsSequence
 *              | SeqEnd
 * JsObject ::= ObjectCell key:String value:JsonItem tail:JsObject
 *             | ObjectEnd
 */
sealed trait JsonItem {
  def convertToString: String =
    this match {
      case JsNumber(v) => v.toString()
      case JsString(v) => s"""${v}"""
      case JsBoolean(v) => if(v) "true" else "false"
      case JsNull => "null"
    }
}
final case class JsNumber(value: Double) extends JsonItem 
final case class JsString(value: String) extends JsonItem 
final case class JsBoolean(value: Boolean) extends JsonItem
final case object JsNull extends JsonItem 
sealed trait JsSequence extends JsonItem {
  override def convertToString: String =
    this match {
      case SeqCell(h, t) => s"${h.convertToString}, ${t.convertToString}"
      case SeqEnd => "]"
    }
}
final case class SeqCell(head: JsonItem, tail: JsSequence) extends JsSequence
final case object SeqEnd extends JsSequence
sealed trait JsObject extends JsonItem {
  override def convertToString: String =
    this match {
      case ObjectCell(k, v, t) => s"${k}: ${v.convertToString} ${t.convertToString}"
      case ObjectEnd => "}"
    }
}
final case class ObjectCell(key: String, value: JsonItem, tail: JsObject) extends JsObject
final case object ObjectEnd extends JsObject
