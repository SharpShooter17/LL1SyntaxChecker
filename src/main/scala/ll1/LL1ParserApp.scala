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

object LL1ParserApp extends App {

  val input = StdIn.readLine("Enter expression: ")

  if (LL1Parser.analyzeSyntax(input)) {
    println("Składnia jest poprawna")
  } else {
    println(s"Składnia nie jest poprawna:")
    println(input)
    val spaces = "~" * LL1Parser.charIndex
    println(s"$spaces^")
  }

}

object LL1Parser {
  private val epsilon = List(Terminal(""))

  private val S = Expression("S")
  private val Z = Expression("Z")
  private val W = Expression("W")
  private val Wprim = Expression("Wprim")
  private val P = Expression("P")
  private val R = Expression("R")
  private val Rprim = Expression("Rprim")
  private val L = Expression("L")
  private val Lprim = Expression("Lprim")
  private val C = Expression("C")
  private val O = Expression("O")

  private val `;` = Terminal(";")
  private val `.` = Terminal(".")
  private val `(` = Terminal("(")
  private val `)` = Terminal(")")
  private val `0` = Terminal("0")
  private val `1` = Terminal("1")
  private val `2` = Terminal("2")
  private val `3` = Terminal("3")
  private val `4` = Terminal("4")
  private val `5` = Terminal("5")
  private val `6` = Terminal("6")
  private val `7` = Terminal("7")
  private val `8` = Terminal("8")
  private val `9` = Terminal("9")
  private val `*` = Terminal("*")
  private val `:` = Terminal(":")
  private val `+` = Terminal("+")
  private val `-` = Terminal("-")
  private val `^` = Terminal("^")

  var charIndex: Int = 0;

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

  def analyzeSyntax(input: String): Boolean = {
    implicit val stack: mutable.Stack[Token] = new mutable.Stack[Token]
    stack.push(S)

    implicit val firstTable: Map[Token, List[Token]] = findFirst()
    showFirstTable(firstTable)

    charIndex = 0

    checkSyntax(input)
  }

  private def checkSyntax(syntax: String)
                         (implicit stack: mutable.Stack[Token], firstTable: Map[Token, List[Token]]): Boolean = {
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

  private def checkTerminal(syntax: String, token: Terminal, analyzingToken: Terminal)
                           (implicit stack: mutable.Stack[Token], firstTable: Map[Token, List[Token]]): Boolean = {
    if (token == analyzingToken) {
      if (syntax.length > 1) {
        charIndex = charIndex + 1
      }
      checkSyntax(syntax.drop(1))
    } else false
  }

  private def checkExpression(syntax: String, token: Expression, analyzingToken: Terminal)
                             (implicit stack: mutable.Stack[Token], firstTable: Map[Token, List[Token]]): Boolean = {
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

      val candidate: List[Token] = candidates.head

      candidate.reverse.foreach(t => stack.push(t))
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