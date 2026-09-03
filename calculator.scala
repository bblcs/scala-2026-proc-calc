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
  // define additional registers here
  var a: Int         = 0
  var b: Int         = 0
  var blink: Boolean = false

  for cmd <- commands do {
    cmd match {
      case "+" => {
        acc = a + b
        blink = false
      }
      case "-" => {
        acc = a - b
        blink = false
      }
      case "*" => {
        acc = a * b
        blink = false
      }
      case "/" => {
        if b != 0 then
          acc = a / b
          blink = false
        else
          a = 0
          b = 0
          blink = false
      }
      case "swap" => {
        val t = a
        a = b
        b = t
      }
      case "blink" => { blink = !blink }
      case "acc" => {
        if !blink then a = acc else b = acc
        blink = !blink
      }
      case other => {
        var number = parseInt(other)
        if !blink then a = number else b = number
        blink = !blink
      }
    }
  }

  println(acc)
}
