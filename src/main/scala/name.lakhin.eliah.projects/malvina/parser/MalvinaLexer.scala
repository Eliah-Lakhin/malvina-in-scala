/*
   Copyright 2013 Ilya Lakhin (Илья Александрович Лахин)

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
package name.lakhin.eliah.projects
package malvina.parser

import name.lakhin.eliah.projects.papacarlo.lexis._
import name.lakhin.eliah.projects.papacarlo.Lexer

object MalvinaLexer {
  private def tokenizer = {
    val tokenizer = new Tokenizer()

    import tokenizer._
    import Matcher._

    tokenCategory(
      "whitespace",
      oneOrMore(anyOf(" \t\f\n"))
    ).skip

    tokenCategory(
      "id",
      sequence(
        rangeOf('a', 'z'),
        zeroOrMore(choice(
          rangeOf('a', 'z'),
          rangeOf('A', 'Z'),
          rangeOf('0', '9')
        ))
      )
    ).mutable

    tokenCategory(
      "Id",
      sequence(
        rangeOf('A', 'Z'),
        rangeOf('a', 'z'),
        zeroOrMore(choice(
          rangeOf('a', 'z'),
          rangeOf('A', 'Z'),
          rangeOf('0', '9')
        ))
      )
    ).mutable

    tokenCategory(
      "type variable",
      sequence(rangeOf('A', 'Z'), zeroOrMore(rangeOf('0', '9')))
    ).mutable

    tokenCategory(
      "string",
      sequence(
        chunk("\""),
        oneOrMore(choice(
          anyExceptOf("\n\r\\\""),
          sequence(chunk("\\"), anyExceptOf("\n\r"))
        )),
        chunk("\"")
      )
    ).mutable

    tokenCategory(
      "hex",
      sequence(
        chunk("0x"),
        zeroOrMore(choice(
          rangeOf('0', '9'),
          rangeOf('a', 'f'),
          rangeOf('A', 'F')
        ))
      )
    ).mutable

    tokenCategory(
      "decimal",
      sequence(oneOrMore(rangeOf('0', '9')))
    ).mutable

    tokenCategory(
      "float",
      sequence(
        oneOrMore(rangeOf('0', '9')),
        chunk("."),
        oneOrMore(rangeOf('0', '9')),
        optional(sequence(
          anyOf("eE"),
          optional(anyOf("+-")),
          oneOrMore(rangeOf('0', '9'))
        ))
      )
    ).mutable

    keywords("true", "false", "null", "lazy", "if", "else", "loop", "break",
      "return", "new", "as", "this", "export", "type", "function", "static",
      "translate", "in", "module")

    terminals("{", "}", "[", "]", "(", ")", ">=", "<=", "==", ">", "<", "=>",
      "->", "=", "+=", "+", "-", "|", "&", ";", ":", "?", "!=", "!", ",", ".",
      "@", "/*", "*/", "*", "/", "//")

    tokenizer
  }

  private def contextualizer = {
    val contextualizer = new Contextualizer

    import contextualizer._

    trackContext("/*", "*/").forceSkip.topContext
    trackContext("//", Token.LineBreakKind).forceSkip.topContext
    trackContext("{", "}").allowCaching

    contextualizer
  }

  def apply() = new Lexer(tokenizer, contextualizer)
}
