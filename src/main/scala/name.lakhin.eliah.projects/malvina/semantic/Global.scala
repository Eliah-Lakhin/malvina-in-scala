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

import name.lakhin.eliah.projects.papacarlo.utils.Registry

final class Global {
  private val descriptions = new Registry[Description]
  private var groups = Map.empty[String, List[Int]]
  private var references = Map.empty[String, Set[Reference]]
  private var affected = Set.empty[Reference]

  def register(description: Description) = {
    val key = description.key
    val id = descriptions.add(description)

    groups += key -> (id :: groups.getOrElse(key, Nil))

    affect(key)

    id
  }

  def deregister(id: Int) {
    for (description <- descriptions.remove(id)) {
      val key = description.key
      val group = groups.getOrElse(key, Nil).filter(_ != id)

      if (group.nonEmpty) groups += key -> group
      else groups -= key

      affect(key)
    }
  }

  def subscribe(name: String, parameters: Int, reference: Reference) {
    val key = Description.key(name, parameters)

    references += key -> (references.getOrElse(key, Set.empty) + reference)
  }

  def unsubscribe(unit: String) {
    references = references.flatMap {
      case (key, subscribers) =>
        val filteredSubscribers = subscribers.filter(_.unit != unit)

        if (filteredSubscribers.nonEmpty) Map(key -> subscribers)
        else Map.empty
    }

    affected = affected.filter(_.unit != unit)
  }

  def unsubscribe(reference: Reference) {
    references = references.flatMap {
      case (key, subscribers) =>
        val filteredSubscribers = subscribers - reference

        if (filteredSubscribers.nonEmpty) Map(key -> subscribers)
        else Map.empty
    }

    affected -= reference
  }

  private def affect(key: String) {
    for (references <- this.references.get(key)) affected ++= references
  }
}
