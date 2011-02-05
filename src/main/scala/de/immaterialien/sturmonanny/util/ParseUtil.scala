package de.immaterialien.sturmonanny.util

import scala.util.parsing.combinator._

trait ParseUtil extends RegexParsers {
  lazy val int: Parser[Int] = {
    """-?\d+[ \t]*""".r ^^ (_.trim toInt)
  }
  lazy val double: Parser[Double] = {
    """-?(?:(?:\d*\.\d+)|\d+)[ \t]*""".r ^^
      (_.trim toDouble)
  }

  def matcher(reg: String): Parser[scala.util.matching.Regex.Match] = matcher(reg.r)
  def matcher(reg: scala.util.matching.Regex): Parser[scala.util.matching.Regex.Match] = new Parser[scala.util.matching.Regex.Match] {
    def apply(in: Input) = {
      val off = in.offset
      (reg findPrefixMatchOf (in.source.subSequence(off, in.source.length))) match {
        case Some(matched) =>
          Success(matched,
            in.drop(matched.end))
        case None =>
          Failure("no match for " + reg + " ", in)
      }
    }
  }

  def direct(str: String): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val off = in.offset
      var i = 0
      var j = off
      while (i < str.length && j < in.source.length && str.charAt(i) == in.source.charAt(j)) {
        i += 1
        j += 1
      }
      val found = in.source.subSequence(off, j).toString
      val r = "".r
      if (i == str.length)
        Success(found, in.drop(j - off))
      else
        Failure("expected '" + str + "' but found '" + found + "'", in)
    }
  }

  def direct(reg: scala.util.matching.Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val off = in.offset
      (reg findPrefixMatchOf (in.source.subSequence(off, in.source.length))) match {
        case Some(matched) =>
          Success(in.source.subSequence(off, off + matched.end).toString,
            in.drop(matched.end))
        case None =>
          Failure("no match for " + reg + " ", in)
      }
    }
  }

}