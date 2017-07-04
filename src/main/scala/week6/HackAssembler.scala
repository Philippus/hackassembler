package week6

import java.io._

import scala.io.Source._

object Main {
  def main(args: Array[String]) {
    if (args.isEmpty)
      for (fileNamePrefix <- List("Add", "Max", "Rect", "Pong")) assemble(fileNamePrefix)
    else
      for (fileNamePrefix <- args.toList) assemble(fileNamePrefix)
  }

  def assemble(fileNamePrefix: String) = {
    val symbolTable = new SymbolTable

    firstPass

    secondPass

    def firstPass = {
      var lineNumber = 0
      for (line <- fromFile("data/in/" + fileNamePrefix + ".asm").getLines)
        if (!lineIsEmptyOrRemark(line)) {
          val cleanLine = removeWhitespaceAndRemarksFromLine(line)
          if (lineIsLabelSymbol(cleanLine)) {
            symbolTable.add(cleanLine.substring(1, cleanLine.length - 1), lineNumber)
          }
          else lineNumber += 1
        }
    }

    def secondPass = {
      var offset = 16
      var instruction = ""
      val resultFileName = fileNamePrefix + ".hack"
      val dir = new File("data/out/")
      if (!dir.exists()) dir.mkdir()
      val resultFile = new PrintWriter("data/out/" + resultFileName)
      for (line <- fromFile("data/in/" + fileNamePrefix + ".asm").getLines)
        if (!lineIsEmptyOrRemark(line)) {
          val cleanLine = removeWhitespaceAndRemarksFromLine(line)
          if (lineIsVarSymbolAndNotDecimal(cleanLine)) {
            if (!symbolTable.contains(cleanLine.trim.split("@").reverse.head)) {
              symbolTable.add(cleanLine.trim.split("@").reverse.head, offset)
              offset += 1
            }
            if (symbolTable.contains(cleanLine.trim.split("@").reverse.head)) {
              instruction = Instruction.getBinaryCodeFromString("@" + symbolTable.getValue(cleanLine.trim.split("@").reverse.head))
            }
            else instruction = Instruction.getBinaryCodeFromString(cleanLine)
            resultFile.print(instruction + "\n")
          }
          else if (!lineIsLabelSymbol(cleanLine)) {
            instruction = Instruction.getBinaryCodeFromString(cleanLine)
            resultFile.print(instruction + "\n")
          }
        }
      resultFile.close
    }
  }

  def lineIsLabelSymbol(line: String): Boolean = {
    line.trim.startsWith("(")
  }

  def lineIsVarSymbolAndNotDecimal(line: String): Boolean = {
    if (line.trim.startsWith("@")) {
      val symbol = line.trim.split("@").reverse.head
      if (symbol forall Character.isDigit) false
      else true
    }
    else false
  }

  def lineIsEmptyOrRemark(line: String): Boolean = {
    if (line.trim.length == 0) true
    else if (line.trim.startsWith("//")) true
    else false
  }

  def removeWhitespaceAndRemarksFromLine(line: String): String = {
    line.split("//").head.trim
  }
}
