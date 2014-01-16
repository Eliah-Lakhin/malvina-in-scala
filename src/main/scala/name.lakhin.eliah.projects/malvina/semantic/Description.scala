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

sealed abstract case class Description(id: Int,
                                       source: Option[Reference],
                                       module: String,
                                       name: String)

final case class TypeDescription(override val id: Int = -1,
                                 override val source: Option[Reference] = None,
                                 override val module: String,
                                 override val name: String,
                                 parameters: Int)
  extends Description(id, source, module, name) {

  override def equals(description: Any) =
    description match {
      case description:TypeDescription =>
        description.module == module &&
          description.name == name &&
          description.parameters == parameters

      case _ => false
    }
}

final case class FunctionDescription(override val id: Int = -1,
                                     override val source: Option[Reference]
                                       = None,
                                     override val module: String,
                                     override val name: String,
                                     variables: List[String],
                                     parameters: List[TypeApplication],
                                     result: TypeApplication)
  extends Description(id, source, module, name) {

  override def equals(description: Any) =
    description match {
      case description:FunctionDescription =>
        description.module == module &&
          description.name == name &&
          description.parameters == parameters
          description.result == result

      case _ => false
    }
}