package week6

object Instruction {

  private def aInstruction(chars: String): String = {
    def toBinary(chars: String, digits: Int = 15): String = {
      String.format("%" + digits + "s", chars.toInt.toBinaryString).replace(' ', '0')
    }
    "0" + toBinary(chars.tail)
  }

  private def cInstruction(chars: String): String = {
    def dest(chars: String): String = {
      chars match {
        case "M" => "001"
        case "D" => "010"
        case "MD" => "011"
        case "A" => "100"
        case "AM" => "101"
        case "AD" => "110"
        case "AMD" => "111"
        case _ => "000"
      }
    }

    def comp(chars: String): String = {
      val c = {
        chars match {
          case "0" => "101010"
          case "1" => "111111"
          case "-1" => "111010"
          case "D" => "001100"
          case "A" | "M" => "110000"
          case "!D" => "001101"
          case "!A" | "!M" => "110001"
          case "-D" => "001111"
          case "-A" | "-M" => "110011"
          case "D+1" => "011111"
          case "A+1" | "M+1" => "110111"
          case "D-1" => "001110"
          case "A-1" | "M-1" => "110010"
          case "D+A" | "D+M" => "000010"
          case "D-A" | "D-M" => "010011"
          case "A-D" | "M-D" => "000111"
          case "D&A" | "D&M" => "000000"
          case "D|A" | "D|M" => "010101"
        }
      }
      val a = chars.exists(_.equals('M'))
      if (a) "1" + c
      else "0" + c
    }

    def jump(chars: String): String = {
      chars match {
        case "JGT" => "001"
        case "JEQ" => "010"
        case "JGE" => "011"
        case "JLT" => "100"
        case "JNE" => "101"
        case "JLE" => "110"
        case "JMP" => "111"
        case _ => "000"
      }
    }
    def hasDestPart(chars: String): Boolean = {
      chars.exists(_.equals('='))
    }
    def hasJumpPart(chars: String): Boolean = {
      chars.exists(_.equals(';'))
    }
    val destPart = {
      if (hasDestPart(chars)) chars.split('=').head
      else ""
    }
    val compPart = {
      if (hasDestPart(chars)) chars.split('=').tail(0).split(';').head
      else chars.split(';').head
    }
    val jumpPart = {
      if (hasJumpPart(chars)) chars.split(';').reverse.head
      else ""
    }

    "111" + comp(compPart) + dest(destPart) + jump(jumpPart)
  }

  def getBinaryCodeFromString(chars: String): String = {
    if (chars.head == '@') aInstruction(chars)
    else cInstruction(chars)
  }
}
