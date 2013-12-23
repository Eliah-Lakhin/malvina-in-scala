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

final class CompilationUnit {
  var log: String = ""

  private val parser = new Parser

  def update(code: String) {
    parser.lexer.input(code)

    val errors = parser.syntax.getErrors

    if (errors.nonEmpty) {
      log += "Errors:\n"
      for (error <- errors)
        log += "  " + error.from + ": " + error.description + "\n"
      log += "\n"
    }
  }

  def getErrors = parser.syntax.getErrors

  for (root <- parser.syntax.getRootNode) {
    root.onAddBranch.bind {
      branch => log += branch.prettyPrint() + "\n\n"
    }
  }
}
