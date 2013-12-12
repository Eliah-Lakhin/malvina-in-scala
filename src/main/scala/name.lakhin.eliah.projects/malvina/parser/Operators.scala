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

private class Operators(val rule: ExpressionRule) {
  def defineInfix(operator: String,
                  function: String,
                  precedence: Int,
                  right: Boolean = false) {
    val leftBindingPower = bindingPower(precedence)
    val rightBindingPower = leftBindingPower - (if (right) 1 else 0)

    rule.parselet(operator)
      .leftBindingPower(leftBindingPower)
      .leftDenotation {
        (expression, left, operatorReference) =>
          val begin = left.getBegin
          var end = operatorReference
          var branches = List.empty[(String, Node)]

          branches :+= argument(left)
          for (right <- expression.parseRight(rightBindingPower)) {
            branches :+= argument(right)
            end = right.getEnd
          }

          Node("application", begin, end, branches,
            List("name" -> operatorReference), Map("name" -> function))
      }
  }

  def definePostfix(operator: String, function: String, precedence: Int) {
    rule.parselet(operator)
      .leftBindingPower(bindingPower(precedence))
      .leftDenotation {
        (expression, left, operatorReference) =>
          Node(
            "application",
            left.getBegin,
            operatorReference,
            List(argument(left)),
            List("name" -> operatorReference),
            Map("name" -> function)
          )
      }
  }

  def definePrefix(operator: String, function: String, precedence: Int) {
    val power = bindingPower(precedence)

    rule.parselet(operator).nullDenotation {
      (expression, operatorReference) => {
        val begin = operatorReference
        var end = operatorReference
        var branches = List.empty[(String, Node)]

        for (right <- expression.parseRight(power)) {
          branches :+= argument(right)
          end = right.getEnd
        }

        Node("application", begin, end, branches,
          List("name" -> operatorReference), Map("name" -> function))
      }
    }
  }

  def defineTernary(function: String, precedence: Int) {
    val conditionBindingPower = bindingPower(precedence)
    val successBranchBindingPower = conditionBindingPower - 1

    rule.parselet("?")
      .leftBindingPower(conditionBindingPower)
      .leftDenotation {
        (expression, condition, operatorReference) =>
          val begin = condition.getBegin
          var end = operatorReference
          var branches = List.empty[(String, Node)]

          branches :+= argument(condition)

          for (success <- expression.parseRight(successBranchBindingPower)) {
            branches :+= argument(success)
            end = success.getEnd
          }

          expression.consume(":")

          for (fail <- expression.parseRight(conditionBindingPower)) {
            branches :+= argument(fail)
            end = fail.getEnd
          }

          Node("application", begin, end, branches,
            List("name" -> operatorReference), Map("name" -> function))
      }
  }

  def defineGroup(tupleConstructor: String) {
    rule.parselet("(").nullDenotation {
      (expression, operatorReference) =>
        val begin = operatorReference
        var end = operatorReference
        var branches = List.empty[Node]

        for (first <- expression.parseRight()) {
          branches ::= first
          end = first.getEnd
        }

        var finished = false
        while (!finished) {
          expression.consume(",", optional = true) match {
            case Some(comma) =>
              end = comma

              for (success <- expression.parseRight()) {
                branches ::= success
                end = success.getEnd
              }

            case None => finished = true
          }
        }

        expression.consume(")")

        branches match {
          case head :: Nil => head

          case _ =>
            Node("application", begin, end, branches.reverse.map(argument),
              List("name" -> operatorReference),
              Map("name" -> tupleConstructor))
        }
    }
  }

  def argument(value: Node) = "argument" -> Node(
    "argument",
    value.getBegin,
    value.getEnd,
    List("value" -> value)
  )

  def bindingPower(precedence: Int) = Int.MaxValue - precedence * 10
}