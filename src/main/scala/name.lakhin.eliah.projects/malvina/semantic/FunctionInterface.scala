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

final class FunctionInterface(unit: Unit, node: Node) extends Member {
  override val phase = "interface"
  private val reference = Reference(unit.name, node.getId, phase)
  private var descriptions = Map.empty[String, (Int, FunctionDescription)]
  private var errors = Set.empty[Int]

  override def resolve() {
    releaseErrors()

    node.getKind match {
      case "function declaration" =>

    }
  }

  override def release() {
    for ((id, description) <- descriptions.values) unit.global.deregister(id)
    releaseErrors()
  }

  private def releaseErrors() {
    errors.foreach(unit.errors.remove)
    errors = Set.empty
  }
}
