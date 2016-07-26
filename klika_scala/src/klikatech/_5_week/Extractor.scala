package klikatech._5_week

import scala.util.matching.Regex
import scala.util.Try

case class Email(localPart: String, domainPart: String)

trait RegexExtractor {
  protected def regex: Regex
  def unapplySeq(s: String): Option[List[String]] = regex.unapplySeq(s)
}

object EmailExtractor extends RegexExtractor {
  protected val regex = """^([^@]+)@([^@]+\.[^@]+)$""".r
}

object URLExtractor extends RegexExtractor {
  protected val regex = """^(?:(https?):\/\/)?([\w-]+(?:\.[\w-]+)+\.?)(?:\/(\S*))?$""".r
}

object HexExtractor extends RegexExtractor {
  protected val regex = "^0[xX]([0-9a-fA-F]+)$".r
}

object Extractors extends App {
  def checkString(s: String) = println(s match {
    case EmailExtractor(name, domain) => s"Email extracted [name:'$name', domain:'$domain']"
    case URLExtractor(protocol, domain, path) => s"URL extracted [protocol:'$protocol', domain:'$domain', path:'$path']"
    case HexExtractor(hex) => s"Hex extracted [value:'$hex']"
    case x => s"Unparsable value provided: $x"
  })

  checkString("arc@gmail.com")
  checkString("https://doepud.co.uk/blog/anatomy-of-a-url")
  checkString("https://en.wikipedia.org/wiki/Uniform_Resource_Locator")
  checkString("0x54fa")
  checkString("blahblah@hotmail.com")
  checkString("arc@gmail.com")
  checkString("https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html")
  checkString("0x0000")
  checkString("abc.cde_22@us.gmail.com")
  checkString("0xIIII")
  checkString("bolibolibo")
}