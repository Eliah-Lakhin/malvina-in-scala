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

import name.lakhin.eliah.projects.malvina.semantic.{Unit, Global}

final class Compiler {
  private var units = Map.empty[String, Unit]
  private val global = new Global

  def input(codes: Map[String, String]) {
    for ((unitName, code) <- codes)
      for (unit <- units.get(unitName).orElse {
        if (code.nonEmpty) {
          val unit = new Unit(unitName, global)
          units += unitName -> unit
          Some(unit)
        }
        else None
      }) {
        unit.update(code)

        if (code.isEmpty) {
          unit.release()
          units -= unitName
        }
      }

    processSemantic()
  }

  def input(unitName: String, code: String) {
    input(Map(unitName -> code))
  }

  private def processSemantic() {

  }
}
