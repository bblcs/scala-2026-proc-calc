import scala.util.boundary, boundary.break

/** Software implementation of PROC (PROstoy Calculator) mk. 1 (or mk. 2).
  *
  * You should finish this procedure according to the reference described in
  * `README.md` to complete the assignment.
  */
@main def calculator(commands: String*): Unit = {

  /** Converts given string `s` to integer.
    *
    * Throws [[NumberFormatException]] if `s` can't be converted to integer, but
    * you shouldn't worry about it at this moment.
    */
  def parseInt(s: String): Int = s.toInt

  /** Representation of `acc` register. */
  var acc: Int = 0

  /** Representations of side registers `a` and `b`. */
  var a: Int = 0
  var b: Int = 0

  /** Representation of `blink` flag. */
  var blink: Boolean = false

  def setAccClearBlink(value: Int): Unit = {
    acc = value
    blink = false
  }

  def updateSideRegister(value: Int): Unit = {
    if !blink then a = value else b = value
    blink = !blink
  }

  boundary {
    for cmd <- commands do {
      cmd match {
        case "+"           => setAccClearBlink(a + b)
        case "-"           => setAccClearBlink(a - b)
        case "*"           => setAccClearBlink(a * b)
        case "/" if b != 0 => setAccClearBlink(a / b)
        case "/"           => { a = 0; b = 0; blink = false }
        case "swap"        => { val t = a; a = b; b = t }
        case "blink"       => { blink = !blink }
        case "acc"         => updateSideRegister(acc)
        case "break"       => break()
        case other         => updateSideRegister(parseInt(other))
      }
    }
  }

  println(acc)
}
