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
package malvina

import name.lakhin.eliah.projects.papacarlo.lexis.{Matcher, Tokenizer}

final class Parser {
  private def tokenizer = {
    val tokenizer = new Tokenizer()

    import tokenizer._
    import Matcher._

    tokenCategory("whitespace", oneOrMore(anyOf(" \t\f\n"))).skip

    tokenCategory(
      "identifier",
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
      "type",
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
      "float number",
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

    tokenCategory(
      "hex number",
      sequence(
        chunk("0x"),
        zeroOrMore(choice(
          rangeOf('0', '9'),
          rangeOf('a', 'f'),
          rangeOf('A', 'F')
        ))
      )
    ).mutable

    tokenCategory("decimal number", zeroOrMore(rangeOf('0', '9'))).mutable

    keywords("true", "false", "null", "lazy", "public", "private", "protected",
      "injection", "class", "singleton", "if", "loop", "break", "return",
      "new", "constructor")

    terminals("{", "}", "[", "]", "(", ")", ">=", "<=", "==", ">", "<", "=>",
      "->", "=", "+=", "+", "-", "|", "&", ";", "*", "%", ":", "?", "!=", "!",
      ",", ".", "@", "$")

    tokenizer
  }
}
