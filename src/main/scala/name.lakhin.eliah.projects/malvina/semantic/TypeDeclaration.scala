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

import name.lakhin.eliah.projects.papacarlo.syntax.Node

final class TypeDeclaration(unit: Unit, node: Node) extends Member {
  override val phase = "type"
  private val reference = Reference(unit.name, node.getId, phase)
  private var description = Option.empty[(Int, TypeDescription)]

  unit.global.affect(reference)

  node.onRemove.bind {_ => release()}

  override def resolve() {
    val description = TypeDescription(
      source = Some(reference),
      module = "this",
      name = node.getValue("name"),
      parameters = node.getValues("variable").size
    )

    this.description match {
      case Some(Pair(id, oldDescription)) =>
        if (oldDescription != description) {
          release()
          this.description =
            Some(unit.global.register(description) -> description)
        }

      case None =>
        this.description =
          Some(unit.global.register(description) -> description)
    }
  }

  override def release() {
    for ((id, description) <- description) unit.global.deregister(id)
  }
}