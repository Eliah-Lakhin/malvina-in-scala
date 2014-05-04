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

protected[malvina] abstract class Member(val unit: Unit, val node: Node) {
  val reference = Reference(unit.name, node.getId, phase)
  protected var errors = Set.empty[Int]
  protected var descriptions = Map.empty[String, (Int, TypeDescription)]

  def phase: String

  def resolve()

  def release() {
    for ((id, description) <- descriptions.values) unit.global.deregister(id)
    releaseErrors()
  }

  protected final def releaseDescriptions(descriptions: Map[String, (Int,
    TypeDescription)]) {
    for ((id, description) <- descriptions.values) unit.global.deregister(id)
  }

  protected final def releaseErrors() {
    errors.foreach(unit.errors.remove)
    errors = Set.empty
  }

  protected final def error(message: String) {
    errors += unit.errors.add(SemanticError(unit.name, node.getId, message))
  }
}

