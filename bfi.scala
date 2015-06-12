// BFI
// Functional Brainfuck interpreter.
// Author: A.J.Pook, 2015

object BFI {
  abstract class \/[+A, +B]
  case class -\/[+A](a: A) extends (A \/ Nothing)
  case class \/-[+B](b: B) extends (Nothing \/ B)
  case class AST (data: List [AST \/ Char])
  case class Memory (p: Int, d: List[Byte])

  def tokenize (source: String): List[Char] = source.toList

  def genAST (tokens: List[Char]): AST = {
    def r (i: Int, c: List [AST \/ Char], p: List [AST \/ Char]): List [AST \/ Char] = {
      (i >= tokens.length) match {
        case true => c
        case false => tokens (i) match {
          case '[' => r (i + 1, List (), c)
          case ']' => val x = p :+ -\/ (AST (c)); r (i + 1, x, x)
          case x @ ('+' | '-' | '<' | '>' | ',' | '.') => r (i + 1, c :+ \/- (x), p)
          case _ => r (i + 1, c, p)
        }
      }
    }
    AST (r (0, List (), List ()))
  }

  def interpret (ast: AST, memory: Memory): Memory = {
    def r (ast: AST, memory: Memory): Memory = {
      memory.d (memory.p) match {
        case 0 => memory
        case _ => r (ast, interpret (ast, memory))
      }
    }
    ast.data.foldLeft (memory) { (m, x) => 
      x match {
        case -\/ (a) => r (a, m)
        case \/- (t) => t match {
          case '>' => m.copy (p = m.p + 1)
          case '<' => m.copy (p = m.p - 1)
          case '+' => m.copy (d = m.d.updated (m.p, (m.d (m.p).toInt + 1).toByte))
          case '-' => m.copy (d = m.d.updated (m.p, (m.d (m.p).toInt - 1).toByte))
          case '.' => Console.out.print (m.d (m.p).toChar); m
          case ',' => m.copy (d = m.d.updated (m.p, Console.in.read.toByte))
        }
      }
    }
  }
}

object DEMO {
  def main(args: Array[String]): Unit = {
    val program =
      """|>+++++++++
         |[<++++++++>-]
         |<.>+++++++
         |[<++++>-]
         |<+.+++++++..+++.>>>++++++++
         |[<++++>-]
         |<.>>>++++++++++
         |[<+++++++++>-]
         |<---.<<<<.+++.------.--------.>>+.
         |""".stripMargin

    println ("\nSOURCE:")
    println (program)

    val tokens = BFI.tokenize (program)
    println ("\nINTERPRETED:")
    val ast = BFI.genAST (tokens)

    BFI.interpret (ast, BFI.Memory (0, List.fill (1024)(0)))
    println ("\n")
  }
}