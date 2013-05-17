import scala.util.matching.Regex

import org.scalatest.FunSuite

import RegexStringContext._

class RegexStringContextTest extends FunSuite {
  def check(expected: Regex, actual: Regex) {
    assert(expected.pattern.pattern === actual.pattern.pattern)
    assert(expected.pattern.flags   === actual.pattern.flags)
  }

  test("plain") {
    check(new Regex("foo"), r"foo")
  }

  test("with metacharacter") {
    check(new Regex("\\w+"), r"\w+")
  }

  test("with plain parameter") {
    val bar = "bar"
    check(new Regex("foo\\Qbar\\E"), r"foo$bar")
  }

  test("with parameter that has metacharacters") {
    val price = "$100.00"
    check(new Regex("Price = \\Q$100.00\\E"), r"Price = $price")
  }

  test("with multiple parameters that have metacharacters") {
    val url = "http://example.com/"
    val foo = "foo"
    check(new Regex("url=\\Qhttp://example.com/\\E,foo=\\Qfoo\\E"), r"url=$url,foo=$foo")
  }

  test("with parameter that includes \\E") {
    val e = "\\E"
    check(new Regex("foo\\Q\\E\\\\E\\Q\\Ebar"), r"foo${e}bar")
  }
}
