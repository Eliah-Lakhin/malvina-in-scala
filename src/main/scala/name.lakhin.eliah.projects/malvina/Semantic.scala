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

import name.lakhin.eliah.projects.papacarlo.utils.Registry

final class Semantic(compiler: Compiler) {
  private val declarations = new Registry[Declaration]
  private var groups = Map.empty[String, Set[Int]]
  private var references = Map.empty[String, Map[String, Set[Int]]]

  def declare(declaration: Declaration) = {
    val id = declarations.add(declaration)
    groups +=
      declaration.key -> (groups.getOrElse(declaration.key, Set.empty) + id)
    affect(declaration.key)
    id
  }

  def free(id: Int) {
    for (declaration <- declarations.get(id)) {
      declarations.remove(id)
      groups +=
        declaration.key -> (groups.getOrElse(declaration.key, Set.empty) - id)
      affect(declaration.key)
    }
  }

  def lookup(name: String, parameters: Int) =
    groups
      .get(Semantic.key(name, parameters))
      .map(_.toList.flatMap(declarations.get))
      .getOrElse(Nil)

  def subscribe(declaration: Declaration, unit: String, memberId: Int) {
    val key = declaration.key
    val subscribers = references.getOrElse(key, Map.empty)
    val members = subscribers.getOrElse(unit, Set.empty) + memberId

    references += key -> (subscribers + (unit -> members))
  }

  def unsubscribe(declaration: Declaration, unit: String, memberId: Int = -1) {
    val key = declaration.key

    for (reference <- references.get(key)) {
      if (memberId >= 0)
        for (members <- reference.get(unit))
          references += key -> (reference + (unit -> (members - memberId)))
      else references += key -> (reference - unit)
    }
  }

  private def affect(key: String) {
    for ((unitName, memberId) <- references.getOrElse(key, Map.empty);
         unit <- compiler.units.get(unitName))
      unit.invalidateMembers(memberId)
  }
}

object Semantic {
  def key(name: String, parameters: Int) = name + "/" + parameters
}