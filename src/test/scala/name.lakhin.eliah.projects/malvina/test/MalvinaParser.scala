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
package malvina.test

import name.lakhin.eliah.projects.papacarlo.test.utils.ParserSpec
import name.lakhin.eliah.projects.malvina.syntax.{Parser, Lexer}

class MalvinaParser extends ParserSpec("parser") {
  override def lexer = Lexer()

  override def parser = {
    val parser = new Parser

    (parser.lexer, parser.syntax)
  }
}