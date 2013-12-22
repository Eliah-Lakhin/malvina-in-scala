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
package malvina.parser

import name.lakhin.eliah.projects.papacarlo.syntax.rules.ExpressionRule
import name.lakhin.eliah.projects.papacarlo.syntax.Node

class TypeSyntax(val rule: ExpressionRule) {
  defineGroup()
  definePostfix(List("?"), "Nullable", 1)
  definePostfix(List("[", "]"), "Array", 1)
  defineInfix("&", "Tuple", 2)
  defineInfix("|", "Variant", 3)
  defineFunction("Function", 4)

  private def defineFunction(typeName: String,
                             precedence: Int) {
    val power = bindingPower(precedence)

    rule.parselet("#").nullDenotation {
      (expression, operatorReference) => {
        val begin = operatorReference
        var end = operatorReference
        var branches = List.empty[(String, Node)]

        expression.consume("(", optional = true) match {
          case Some(openParenReference) =>
            end = openParenReference

            var finished = false
            while (!finished) {
              expression.parseRight(power) match {
                case Some(parameter) =>
                  end = parameter.getEnd
                  branches ::= "parameter" -> parameter

                  expression.consume(",", optional = true) match {
                    case Some(commaReference) => end = commaReference
                    case None => finished = true
                  }

                case None => finished = true
              }
            }
            for (reference <- expression.consume("=>")) end = reference

            for (result <- expression.parseRight(power)) {
              end = result.getEnd
              branches ::= "parameter" -> result
            }

            for (reference <- expression.consume(")")) end = reference

          case None =>
            for (result <- expression.parseRight(power)) {
              end = result.getEnd
              branches ::= "parameter" -> result
            }
        }

        Node("application", begin, end, branches.reverse,
          List("name" -> operatorReference), Map("name" -> typeName))
      }
    }
  }

  private def definePostfix(operators: List[String],
                            typeName: String,
                            precedence: Int) {
    for (head <- operators.headOption)
      rule.parselet(head)
        .leftBindingPower(bindingPower(precedence))
        .leftDenotation {
          (expression, left, operatorReference) =>
            var lastReference = operatorReference

            for (rest <- operators.tail;
                 reference <- expression.consume(rest))
              lastReference = reference

            Node(
              "type application",
              left.getBegin,
              lastReference,
              branches = List("parameter" -> left),
              references = List("name" -> operatorReference),
              constants = Map("name" -> typeName)
            )
        }
  }

  private def defineInfix(operator: String,
                          typeName: String,
                          precedence: Int) {
    val power = bindingPower(precedence)

    rule.parselet(operator)
      .leftBindingPower(power)
      .leftDenotation {
        (expression, left, operatorReference) =>
          val begin = left.getBegin
          var end = operatorReference
          var branches = List("parameter" -> left)

          for (right <- expression.parseRight(power)) {
            branches ::= "parameter" -> right
            end = right.getEnd
          }

          var finished = false
          while (!finished) {
            expression.consume(operator, optional = true) match {
              case Some(reference) =>
                end = reference

                expression.parseRight(power) match {
                  case Some(parameter) =>
                    end = parameter.getEnd
                    branches ::= "parameter" -> parameter

                  case None => finished = true
                }

              case None => finished = true
            }
          }

          Node("type application", begin, end, branches.reverse,
            List("name" -> operatorReference),
            Map("name" -> typeName, "operator" -> operator)
          )
      }
  }

  private def defineGroup() {
    rule.parselet("(").nullDenotation {
      (expression, operatorReference) =>
        val value = expression.parseRight().getOrElse(expression.placeholder)

        value
          .accessor
          .setBegin(operatorReference)
          .setEnd(expression.consume(")").getOrElse(value.getEnd))
          .setConstant("operator", "group")
          .node
    }
  }

  private def bindingPower(precedence: Int) = Int.MaxValue - precedence * 10
}
