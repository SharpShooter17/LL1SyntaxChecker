package ll1

import ll1.TokenType.TokenType

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.io.StdIn
import scala.language.existentials

object TokenType extends Enumeration {
  type TokenType = Value
  val TerminalToken, ExpressionToken = Value
}

trait Token {
  def tokenType: TokenType

  def name: String
}

case class Terminal(token: String) extends Token {
  override def tokenType: TokenType = TokenType.TerminalToken

  override def name: String = token
}

case class Expression(token: String) extends Token {
  override def tokenType: TokenType = TokenType.ExpressionToken

  override def name: String = token
}

object LL1Parser extends App {

  val epsilon = List(Terminal(""))

  val S = Expression("S")
  val Z = Expression("Z")
  val W = Expression("W")
  val Wprim = Expression("Wprim")
  val P = Expression("P")
  val R = Expression("R")
  val Rprim = Expression("Rprim")
  val L = Expression("L")
  val Lprim = Expression("Lprim")
  val C = Expression("C")
  val O = Expression("O")

  val `;` = Terminal(";")
  val `.` = Terminal(".")
  val `(` = Terminal("(")
  val `)` = Terminal(")")
  val `0` = Terminal("0")
  val `1` = Terminal("1")
  val `2` = Terminal("2")
  val `3` = Terminal("3")
  val `4` = Terminal("4")
  val `5` = Terminal("5")
  val `6` = Terminal("6")
  val `7` = Terminal("7")
  val `8` = Terminal("8")
  val `9` = Terminal("9")
  val `*` = Terminal("*")
  val `:` = Terminal(":")
  val `+` = Terminal("+")
  val `-` = Terminal("-")
  val `^` = Terminal("^")

  private val grammar: ListMap[Token, Set[List[Token]]] = ListMap(
    S -> Set(List(W, `;`, Z)),
    Z -> Set(List(W, `;`, Z), epsilon),
    W -> Set(List(P, Wprim)),
    Wprim -> Set(List(O, W), epsilon),
    P -> Set(List(R), List(`(`, W, `)`)),
    R -> Set(List(L, Rprim)),
    Rprim -> Set(List(`.`, L), epsilon),
    L -> Set(List(C, Lprim)),
    Lprim -> Set(List(L), epsilon),
    C -> Set(List(`0`), List(`1`), List(`2`), List(`3`), List(`4`), List(`5`), List(`6`), List(`7`), List(`8`), List(`9`)),
    O -> Set(List(`*`), List(`:`), List(`+`), List(`-`), List(`^`))
  )

  val stack = new mutable.Stack[Token]
  stack.push(S)

  //  val input = "(1.2*3)+5-(23.4+3)^3;8:13;"
  val input = StdIn.readLine("Enter expression: ")
  val firstTable: Map[Token, List[Token]] = findFirst()

  showFirstTable(firstTable)
  val isValid = checkSyntax(input)
  if (isValid && stack.isEmpty) {
    println("Składnia jest poprawna")
  } else {
    println(s"Składnia nie jest poprawna")
  }

  private def checkSyntax(syntax: String): Boolean = {
    if (stack.isEmpty && syntax.isEmpty) {
      return true
    } else if (stack.isEmpty) {
      return false
    }

    val token: Token = stack.pop()
    val analyzingToken = Terminal(syntax.take(1))

    token.tokenType match {
      case TokenType.TerminalToken => checkTerminal(syntax, token.asInstanceOf[Terminal], analyzingToken)
      case TokenType.ExpressionToken => checkExpression(syntax, token.asInstanceOf[Expression], analyzingToken)
    }
  }

  private def checkTerminal(syntax: String, token: Terminal, analyzingToken: Terminal): Boolean = {
    val isEquals = token == analyzingToken
    isEquals && checkSyntax(syntax.drop(1))
  }

  private def checkExpression(syntax: String, token: Expression, analyzingToken: Terminal): Boolean = {
    val sentence: Set[List[Token]] = grammar(token)
    if (firstTable(token).contains(analyzingToken)) {
      val candidates = sentence.filter(
        alternative => alternative.headOption.exists { token =>
          token.tokenType match {
            case TokenType.TerminalToken => token == analyzingToken
            case TokenType.ExpressionToken => firstTable(token).contains(analyzingToken)
          }
        }
      )

      if (candidates.size != 1) {
        throw new IllegalStateException("Cannot found one way! Check grammar!")
      }

      val candidate = candidates.head

      candidate.reverse.foreach(stack.push)
      checkSyntax(syntax)
    } else if (sentence.contains(epsilon)) {
      checkSyntax(syntax)
    } else {
      false
    }
  }

  private def findFirst(): Map[Token, List[Token]] = {
    grammar.keys.toList.reverse.map { expression =>
      (expression, findFirstFor(expression))
    }.toMap
  }

  private def findFirstFor(expression: Token): List[Token] = {
    grammar(expression).flatMap(_.headOption)
      .flatMap { token =>
        token.tokenType match {
          case TokenType.TerminalToken => List(token)
          case TokenType.ExpressionToken => findFirstFor(token.asInstanceOf[Expression])
        }
      }.toList
  }

  private def showFirstTable(table: Map[Token, List[Token]]): Unit = {
    println("FIRST TABLE")
    println(
      table
        .map(first => s"FIRST(${first._1.name}) = {${first._2.map(_.name).sorted.mkString(", ")}}")
        .mkString("\n")
    )
  }

}
