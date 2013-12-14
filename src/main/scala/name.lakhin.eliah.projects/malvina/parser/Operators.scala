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

import name.lakhin.eliah.projects.papacarlo.syntax.rules.{RecoveryRule,
  ExpressionRule}
import name.lakhin.eliah.projects.papacarlo.syntax.Node

private class Operators(val rule: ExpressionRule) {
  def define() {
    defineGroup("tuple")
    defineChain(".", 1)
    defineAccessor("access", 2)
    definePostfix("++", "next", 3)
    definePostfix("--", "previous", 3)
    definePrefix("!", "not", 4)
    definePrefix("-", "minus", 4)
    defineInfix("*", "multiply", 5)
    defineInfix("/", "divide", 5)
    defineInfix("+", "plus", 6)
    defineInfix("-", "minus", 6)
    defineInfix("->", "arrow", 7)
    defineGreater(">", "greater", "bind", "unbind", 8)
    defineInfix(">=", "greaterOrEqual", 8, right = true)
    defineInfix("<", "lesser", 8, right = true)
    defineInfix("<=", "lesserOrEqual", 8, right = true)
    defineInfix("==", "equal", 9, right = true)
    defineInequality("!=", "not", "equal", 9)
    defineInfix("&", "and", 10)
    defineInfix("|", "or", 10)
    defineTernary("condition", 15)
    defineEquality("=", 17)
  }

  private def defineAccessor(accessFunction: String, precedence: Int) {
    val dotBindingPower = bindingPower(precedence)

    rule.parselet("[")
      .leftBindingPower(dotBindingPower)
      .leftDenotation {
        (expression, left, operatorReference) =>
          val begin = left.getBegin
          var end = operatorReference
          var index = List.empty[(String, Node)]

          expression.saveState()
          expression.parseRight() match {
            case Some(first) =>
              if (first.getKind == RecoveryRule.PlaceholderKind)
                expression.restoreState()
              else {
                index ::= "argument" -> first
                end = first.getEnd
                expression.freeState()
              }

            case None =>
              expression.freeState()
          }

          var finished = index.isEmpty
          while (!finished) {
            expression.consume(",", optional = true) match {
              case Some(comma) =>
                end = comma

                for (success <- expression.parseRight()) {
                  index ::= "argument" -> success
                  end = success.getEnd
                }

              case None => finished = true
            }
          }

          end = expression.consume("]").getOrElse(end)

          Node("application", begin, end, ("argument" -> left) :: index.reverse,
            List("name" -> operatorReference),
            Map("name" -> accessFunction))
      }
  }

  private def defineChain(dotOperator: String, precedence: Int) {
    val dotBindingPower = bindingPower(precedence)

    rule.parselet(dotOperator)
      .leftBindingPower(dotBindingPower)
      .leftDenotation {
        (expression, left, operatorReference) =>
          expression.parseRight(dotBindingPower) match {
            case Some(right: Node) if right.getKind == "application" =>
              right
                .accessor
                .setBegin(left.getBegin)
                .setBranches("argument", left :: right.getBranches("argument"))
                .node

            case Some(right: Node) if right.getKind == "variable" &&
              !right.hasBranch("value") =>

              Node("application", left.getBegin, right.getEnd,
                List("argument" -> left),
                List("name" -> right.getBegin))

            case Some(right: Node) =>
              Node("application", left.getBegin, right.getEnd,
                List("argument" -> left, "argument" -> right),
                List("name" -> operatorReference), Map("name" -> "define"))

            case None => left
          }
      }
  }

  private def defineEquality(equalOperator: String, equalPrecedence: Int) {
    val equalLeftBindingPower = bindingPower(equalPrecedence)
    val equalRightBindingPower = equalLeftBindingPower - 1

    rule.parselet(equalOperator)
      .leftBindingPower(equalLeftBindingPower)
      .leftDenotation {
        (expression, left, operatorReference) =>
          expression.parseRight(equalRightBindingPower) match {
            case Some(right) => left.getKind match {
              case "application" =>
                left
                  .accessor
                  .setEnd(right.getEnd)
                  .setBranches("argument", left.getBranches("argument") :+
                    right)
                  .node

              case "variable" if !left.hasBranch("value") =>
                left
                  .accessor
                  .setEnd(right.getEnd)
                  .setBranches("value", List(right))
                  .node

              case _ =>
                Node("application", left.getBegin, right.getEnd,
                  List("argument" -> left, "argument" -> right),
                  List("name" -> operatorReference), Map("name" -> "define"))
            }

            case None => left
          }
      }
  }

  private def defineGreater(greaterOperator: String,
                            greaterFunction: String,
                            bindFunction: String,
                            unbindFunction: String,
                            precedence: Int) {
    val leftBindingPower = bindingPower(precedence)
    val rightBindingPower = leftBindingPower - 1

    rule.parselet(greaterOperator)
      .leftBindingPower(leftBindingPower)
      .leftDenotation {
        (expression, left, operatorReference) =>
          val begin = left.getBegin
          var end = operatorReference
          var function = greaterFunction
          var freeState = true
          var branches = List("argument" -> left)

          expression.saveState()
          expression.consume("!", optional = true) match {
            case Some(exclamation) =>
              expression.consume(">", optional = true) match {
                case Some(secondGreater) =>
                  end = secondGreater
                  function = unbindFunction
                  freeState = false
                case None =>
              }

            case None =>
              expression.consume(">", optional = true) match {
                case Some(secondGreater) =>
                  end = secondGreater
                  function = bindFunction
                  freeState = false
                case None =>
              }
          }

          if (freeState) expression.freeState()

          for (right <- expression.parseRight(rightBindingPower)) {
            branches :+= "argument" -> right
            end = right.getEnd
          }

          Node("application", begin, end, branches,
            List("name" -> operatorReference), Map("name" -> function))
      }
  }

  private def defineInequality(operator: String,
                               notFunction: String,
                               equalFunction: String,
                               precedence: Int) {
    val leftBindingPower = bindingPower(precedence)
    val rightBindingPower = leftBindingPower - 1

    rule.parselet(operator)
      .leftBindingPower(leftBindingPower)
      .leftDenotation {
        (expression, left, operatorReference) =>
          val begin = left.getBegin
          var end = operatorReference
          var branches = List("argument" -> left)

          for (right <- expression.parseRight(rightBindingPower)) {
            branches :+= "argument" -> right
            end = right.getEnd
          }

          val equalApplication = Node("application", begin, end, branches,
            List("name" -> operatorReference), Map("name" -> equalFunction))

          Node("application", begin, end, List("argument" -> equalApplication),
            List("name" -> operatorReference), Map("name" -> notFunction))
      }
  }

  private def defineInfix(operator: String,
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
          var branches = List("argument" -> left)

          for (right <- expression.parseRight(rightBindingPower)) {
            branches :+= "argument" -> right
            end = right.getEnd
          }

          Node("application", begin, end, branches,
            List("name" -> operatorReference), Map("name" -> function))
      }
  }

  private def definePostfix(operator: String,
                            function: String,
                            precedence: Int) {
    rule.parselet(operator)
      .leftBindingPower(bindingPower(precedence))
      .leftDenotation {
        (expression, left, operatorReference) =>
          Node(
            "application",
            left.getBegin,
            operatorReference,
            List("argument" -> left),
            List("name" -> operatorReference),
            Map("name" -> function)
          )
      }
  }

  private def definePrefix(operator: String,
                           function: String,
                           precedence: Int) {
    val power = bindingPower(precedence)

    rule.parselet(operator).nullDenotation {
      (expression, operatorReference) => {
        val begin = operatorReference
        var end = operatorReference
        var branches = List.empty[(String, Node)]

        for (right <- expression.parseRight(power)) {
          branches = List("argument" -> right)
          end = right.getEnd
        }

        Node("application", begin, end, branches,
          List("name" -> operatorReference), Map("name" -> function))
      }
    }
  }

  private def defineTernary(function: String, precedence: Int) {
    val conditionBindingPower = bindingPower(precedence)
    val successBranchBindingPower = conditionBindingPower - 1

    rule.parselet("?")
      .leftBindingPower(conditionBindingPower)
      .leftDenotation {
        (expression, condition, operatorReference) =>
          val begin = condition.getBegin
          var end = operatorReference
          var branches = List.empty[(String, Node)]

          branches :+= "argument" -> condition

          for (success <- expression.parseRight(successBranchBindingPower)) {
            branches :+= "argument" -> success
            end = success.getEnd
          }

          expression.consume(":")

          for (fail <- expression.parseRight(conditionBindingPower)) {
            branches :+= "argument" -> fail
            end = fail.getEnd
          }

          Node("application", begin, end, branches,
            List("name" -> operatorReference), Map("name" -> function))
      }
  }

  private def defineGroup(tupleConstructor: String) {
    rule.parselet("(").nullDenotation {
      (expression, operatorReference) =>
        val begin = operatorReference
        var end = operatorReference
        var branches = List.empty[(String, Node)]

        expression.saveState()
        expression.parseRight() match {
          case Some(first) =>
            if (first.getKind == RecoveryRule.PlaceholderKind)
              expression.restoreState()
            else {
              branches ::= "argument" -> first
              end = first.getEnd
              expression.freeState()
            }

          case None =>
            expression.freeState()
        }

        var finished = branches.isEmpty
        while (!finished) {
          expression.consume(",", optional = true) match {
            case Some(comma) =>
              end = comma

              for (success <- expression.parseRight()) {
                branches ::= "argument" -> success
                end = success.getEnd
              }

            case None => finished = true
          }
        }

        end = expression.consume(")").getOrElse(end)

        branches match {
          case head :: Nil =>
            head._2
              .accessor
              .setBegin(begin)
              .setEnd(end)
              .node

          case _ =>
            Node("application", begin, end, branches.reverse,
              List("name" -> operatorReference),
              Map("name" -> tupleConstructor))
        }
    }
  }

  private def bindingPower(precedence: Int) = Int.MaxValue - precedence * 10
}