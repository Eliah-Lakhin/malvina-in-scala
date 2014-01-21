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

import name.lakhin.eliah.projects.malvina.syntax.Parser
import name.lakhin.eliah.projects.malvina.semantic._

final class Unit(name: String, global: Global) {
  private val parser = new Parser
  private var members = Map.empty[Int, List[Member]]

  def update(code: String) {
    parser.lexer.input(code)
  }

  for (root <- parser.syntax.getRootNode)
    root.onAddBranch.bind {
      branch =>
        branch.getKind match {
          case "type declaration" =>
            var nodeMembers = List(new TypeDeclaration(name, branch, global))

            if (branch.hasBranch("body")) {
            }

            members += branch.getId -> nodeMembers

          case _ =>
        }
    }

  private[Compiler] def resolve(references: List[Reference]) {
    for (reference <- references;
         nodeMembers <- members.get(reference.node);
         member <- nodeMembers.filter(_.phase == reference.kind))
      member.resolve()
  }
}