package ll1

import org.junit.jupiter.api.Test

class LL1ParserTest {

  @Test
  def analyzerTest(): Unit = {
    assert(LL1Parser.analyzeSyntax("(1.2*3)+5-(23.4+3)^3;8:13;"))
    assert(LL1Parser.analyzeSyntax("((1.2)*3)+5-(23.4+3)^3;8:13;"))

    assert(!LL1Parser.analyzeSyntax("(1.2*3)+5-(23.4+3)^3;8:13;;") && LL1Parser.charIndex == 26)
    assert(!LL1Parser.analyzeSyntax("(1..2*3)+5-(23.4+3)^3;8:13;") && LL1Parser.charIndex == 3)
    assert(!LL1Parser.analyzeSyntax("(1.2*+3)+5-(23.4+3)^3;8:13;") && LL1Parser.charIndex == 5)
    assert(!LL1Parser.analyzeSyntax("(1.2*3))+5-(23.4+3)^3;8:13;") && LL1Parser.charIndex == 7)
    assert(!LL1Parser.analyzeSyntax("(1.2*3)+5-(23.4+3)^3;8:13;1") && LL1Parser.charIndex == 26)
    assert(!LL1Parser.analyzeSyntax("(1.2*3)+5-(23.4+3)^3;8:13;1+") && LL1Parser.charIndex == 27)
    assert(!LL1Parser.analyzeSyntax("(1.2*3)+5-(23.4+3)^3;8:13;1+;") && LL1Parser.charIndex == 28)
  }

}
