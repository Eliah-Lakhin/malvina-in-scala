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
package malvina.semantic

import name.lakhin.eliah.projects.malvina.syntax.Parser
import name.lakhin.eliah.projects.papacarlo.utils.Registry

final class Unit(val name: String, val global: Global) {
  private val parser = new Parser
  private var members = Map.empty[Int, Map[String, Member]]
  private[semantic] var errors = new Registry[SemanticError]

  def update(code: String) {
    parser.lexer.input(code)
  }

  for (root <- parser.syntax.getRootNode)
    root.onAddBranch.bind {
      branch =>
        branch.getKind match {
          case "type declaration" =>
            var members = Map(pair(new TypeDeclaration(this, branch)))

            if (branch.hasBranch("body"))
              members += pair(new FunctionInterface(this, branch))

            this.members += branch.getId -> members

          case "function declaration" =>


          case _ =>
        }
    }

  private def pair(member: Member) = member.phase -> member

  private[Compiler] def release() {
    for (member <- members.values.flatten) member._2.release()
    errors = new Registry[SemanticError]
  }

  private[Compiler] def resolve(references: List[Reference]) {
    for (reference <- references;
         member <- members
           .get(reference.nodeId)
           .flatMap(_.get(reference.phase)))
      member.resolve()
  }
}