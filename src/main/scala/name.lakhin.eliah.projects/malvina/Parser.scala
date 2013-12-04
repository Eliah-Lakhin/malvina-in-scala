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

import name.lakhin.eliah.projects.papacarlo.lexis.{Token, Contextualizer,
  Matcher, Tokenizer}
import name.lakhin.eliah.projects.papacarlo.{Syntax, Lexer}
import name.lakhin.eliah.projects.papacarlo.syntax.{NodeAccessor, Node, Rule}
import name.lakhin.eliah.projects.papacarlo.syntax.rules.{NamedRule, ExpressionRule, ChoiceRule}

final class Parser {
  private def tokenizer = {
    val tokenizer = new Tokenizer()

    import tokenizer._
    import Matcher._

    tokenCategory(
      "whitespace",
      oneOrMore(anyOf(" \t\f\n"))
    ).skip

    tokenCategory(
      "id",
      sequence(
        rangeOf('a', 'z'),
        zeroOrMore(choice(
          rangeOf('a', 'z'),
          rangeOf('A', 'Z'),
          rangeOf('0', '9')
        ))
      )
    ).mutable

    tokenCategory(
      "Id",
      sequence(
        rangeOf('A', 'Z'),
        rangeOf('a', 'z'),
        zeroOrMore(choice(
          rangeOf('a', 'z'),
          rangeOf('A', 'Z'),
          rangeOf('0', '9')
        ))
      )
    ).mutable

    tokenCategory(
      "type variable",
      sequence(rangeOf('A', 'Z'), zeroOrMore(rangeOf('0', '9')))
    ).mutable

    tokenCategory(
      "string",
      sequence(
        chunk("\""),
        oneOrMore(choice(
          anyExceptOf("\n\r\\\""),
          sequence(chunk("\\"), anyExceptOf("\n\r"))
        )),
        chunk("\"")
      )
    ).mutable

    tokenCategory(
      "hex",
      sequence(
        chunk("0x"),
        zeroOrMore(choice(
          rangeOf('0', '9'),
          rangeOf('a', 'f'),
          rangeOf('A', 'F')
        ))
      )
    ).mutable

    tokenCategory(
      "decimal",
      sequence(oneOrMore(rangeOf('0', '9')))
    ).mutable

    tokenCategory(
      "float",
      sequence(
        oneOrMore(rangeOf('0', '9')),
        chunk("."),
        oneOrMore(rangeOf('0', '9')),
        optional(sequence(
          anyOf("eE"),
          optional(anyOf("+-")),
          oneOrMore(rangeOf('0', '9'))
        ))
      )
    ).mutable

    keywords("true", "false", "null", "lazy", "if", "else", "loop", "break",
      "return", "new", "as", "this", "export", "type", "function", "static",
      "translate", "in", "module")

    terminals("{", "}", "[", "]", "(", ")", ">=", "<=", "==", ">", "<", "=>",
      "->", "=", "+=", "+", "-", "|", "&", ";", "%", ":", "?", "!=", "!", ",",
      ".", "@", "/*", "*/", "*", "/", "//")

    tokenizer
  }

  private def contextualizer = {
    val contextualizer = new Contextualizer

    import contextualizer._

    trackContext("/*", "*/").forceSkip.topContext
    trackContext("//", Token.LineBreakKind).forceSkip.topContext
    trackContext("{", "}").allowCaching

    contextualizer
  }

  val lexer = new Lexer(tokenizer, contextualizer)

  def syntax = new {
    val syntax = new Syntax(lexer)

    import syntax._
    import Rule._

    mainRule("compilation unit") {
      oneOrMore(branch("declaration", declaration))
    }

    val declaration = rule("declaration") {
      sequence(
        optional(capture("export", token("export"))),
        choice(functionDeclaration, typeDeclaration, staticDeclaration,
          translator, moduleDefinition)
      )
    }

    val functionReference = rule("function reference") {
      sequence(
        capture("name", token("id")),
        optional(moduleReference),
        optional(sequence(
          capture("types", token("(")),
          zeroOrMore(branch("type", token("Id")), token(",")),
          token(")").permissive
        ))
      )
    }

    val typeReference = rule("type reference") {
      sequence(
        capture("name", token("Id")),
        optional(moduleReference),
        optional(sequence(
          token("<"),
          capture("parameters", token("decimal")),
          token(">").permissive
        ))
      )
    }

    val constructor = rule("constructor") {
      sequence(
        branch("parameters", methodParameters),
        branch("body", constructorBody)
      )
    }

    val constructorBody = rule("constructor body") {
      sequence(
        token("{"),
        zeroOrMore(branch("statement", choice(propertyDefinition,
          binaryBranching, multipleBranching, loop, returnStatement,
          breakStatement, variableDefinition, expressionStatement))),
        token("}").permissive
      )
    }

    cachable(constructorBody)

    val propertyDefinition = rule("property") {
      sequence(
        token("this"),
        token("."),
        capture("name", token("id")),
        token(":"),
        branch("type", typeApplication),
        token("=").permissive,
        branch("value", expression).permissive,
        token(";").permissive
      )
    }

    val typePattern = rule("type pattern") {
      sequence(
        token("<"),
        oneOrMore(capture("variable", token("type variable")), token(",")),
        token(">").permissive
      )
    }

    val methodParameters: NamedRule = rule("method parameters") {
      sequence(
        token("("),
        zeroOrMore(branch("parameter", methodParameter), token(",").permissive),
        token(")").permissive
      )
    }

    val methodParameter = rule("method parameter") {
      sequence(
        optional(capture("lazy", token("lazy"))),
        capture("name", token("id")),
        sequence(
          token(":"),
          branch("type", typeApplication)
        ).permissive,
        optional(sequence(
          token("="),
          branch("value", expression)
        ))
      )
    }

    val typeApplication: NamedRule = rule("type application") {
      sequence(
        capture("type", choice(token("Id"), token("type variable"))),
        optional(moduleReference),
        optional(branch("parameters", typeParameters))
      )
    }

    val typeParameters: NamedRule = rule("type parameters") {
      sequence(
        token("<"),
        oneOrMore(branch("parameter", typeApplication.permissive), token(",")),
        token(">").permissive
      )
    }

    val expression: NamedRule = rule("expression") {
      val operators = new Operators(Rule.expression(branch("operand",
        operand.permissive("operand required"))))

      operators.rule
    }

    val array = rule("array") {
      sequence(
        token("["),
        zeroOrMore(branch("element", expression), token(",")),
        token("]").permissive
      )
    }

    val integer = rule("integer") {
      capture("value", choice(token("decimal"), token("hex")))
    }

    intercept(integer) {
      _.accessor
        .setKind("application")
        .setConstant("name", "integer")
        .node
    }

    val float = rule("float") {
      capture("value", token("float"))
    }

    intercept(float) {
      _.accessor
        .setKind("application")
        .setConstant("name", "float")
        .node
    }

    val string = rule("string") {
      capture("value", token("string"))
    }

    intercept(string) {
      _.accessor
        .setKind("application")
        .setConstant("name", "string")
        .node
    }

    val nullLiteral = rule("null") {
      capture("name", token("null"))
    }

    intercept(nullLiteral) {
      _.accessor
        .setKind("application")
        .setConstant("name", "nullable")
        .node
    }

    val thisReference = rule("this") {token("this")}

    val application = rule("application") {
      sequence(
        capture("name", token("id")),
        optional(sequence(
          token("@"),
          capture("package", token("name")).permissive
        )),
        token("("),
        zeroOrMore(branch("argument", argument)),
        token(")").permissive
      )
    }

    val argument = rule("argument") {
      sequence(
        optional(sequence(capture("name", token("token")), token(":"))),
        branch("value", expression)
      )
    }

    val variable = rule("variable") {
      capture("name", token("id"))
    }

    val function = rule("function") {
      sequence(
        optional(branch("parameters", choice(functionParameters,
          singleParameter))),
        token("=>"),
        branch("body", expressionBody)
      )
    }

    val functionParameters = rule("function parameters") {
      sequence(
        token("("),
        branch("parameter", oneOrMore(functionParameter, token(","))),
        token(")").permissive
      )
    }

    val functionParameter = rule("function parameter") {
      sequence(
        capture("name", token("id")),
        optional(sequence(
          token(":"),
          capture("type", typeApplication)
        ))
      )
    }

    val singleParameter = rule("single parameter") {
      capture("name", token("id"))
    }

    intercept(singleParameter) {
      single =>
        Node(
          "function parameters", single.getBegin, single.getEnd,
          branches = List("parameter" ->
            single.accessor.setKind("function parameter").node)
        )
    }

    val expressionBody = rule("expression body") {
      branch("expression", expression)
    }

    intercept(expressionBody) {
      expression =>
        Node("block", expression.getBegin, expression.getEnd, branches = List(
          "statement" -> expression.accessor.setKind("expression statement")
            .node
        ))
    }

    val block: NamedRule = rule("block") {
      sequence(
        token("{"),
        zeroOrMore(branch("statement", choice(binaryBranching,
          multipleBranching, loop, returnStatement, breakStatement,
          variableDefinition, expressionStatement))),
        token("}").permissive
      )
    }

    cachable(block)

    val binaryBranching: NamedRule = rule("binary branching") {
      sequence(
        token("if"),
        token("(").permissive,
        branch("conditions", conditions),
        token(")").permissive,
        branch("success", bodyStatement),
        optional(sequence(
          token("else"),
          branch("fail", bodyStatement)
        ))
      )
    }

    val conditions = rule("conditions") {
       oneOrMore(
         sequence(
           branch("expression", expression),
           optional(sequence(token("as"), capture("variable", token("id"))))
         ),
         separator = token(",")
       )
    }

    val multipleBranching: NamedRule = rule("multiple branching") {
      sequence(
        token("if"),
        token("{").permissive,
        oneOrMore(sequence(
          branch("conditions", choice(conditions, defaultCondition)),
          token(":").permissive,
          branch("body", bodyStatement)
        )),
        token("}").permissive
      )
    }

    val loop: NamedRule = rule("loop") {
      sequence(
        token("loop"),
        token("(").permissive,
        branch("conditions", conditions),
        token(")").permissive,
        branch("body", bodyStatement)
      )
    }

    val returnStatement = rule("return") {
      sequence(
        token("return"),
        optional(capture("value", expression)),
        token(";").permissive
      )
    }

    val breakStatement = rule("break") {
      sequence(token("break"), token(";").permissive)
    }

    val variableDefinition = rule("variable definition") {
      sequence(
        capture("name", token("name")),
        token(":"),
        optional(capture("type", typeApplication)),
        token("=").permissive,
        branch("value", expression).permissive,
        token(";").permissive
      )
    }

    val expressionStatement = rule("expression statement") {
      sequence(
        branch("expression", expression),
        token(";").permissive
      ).required
    }

    val defaultCondition = rule("default condition") {token("else")}

    val operand = name("operand") {
      choice(array, integer, float, string, nullLiteral, thisReference,
        application, function, variable)
    }

    val bodyStatement = name("body statement") {
      choice(block, binaryBranching, multipleBranching, loop, returnStatement,
        breakStatement, expressionStatement)
    }

    val moduleReference = name("module reference") {
      sequence(token("@"), capture("module", token("id")))
    }

    val functionDeclaration = name("function declaration") {
      sequence(
        capture("declaration", token("function")),
        capture("name", token("id")),
        branch("parameters", methodParameters),
        optional(sequence(token(":"), branch("result", typeApplication))),
        choice(branch("body", block), token(";").permissive)
      )
    }

    val typeDeclaration = name("type declaration") {
      sequence(
        capture("declaration", token("type")),
        capture("name", token("Id")),
        optional(branch("pattern", typePattern)),
        choice(branch("constructor", constructor), token(";").permissive)
      )
    }

    val staticDeclaration = name("static declaration") {
      sequence(
        capture("declaration", token("static")),
        capture("name", token("Id")),
        sequence(token(":"), branch("result", typeApplication)).permissive,
        sequence(token("="), branch("value", expression)).permissive,
        token(";").permissive
      )
    }

    val translator = name("translator") {
      sequence(
        capture("declaration", token("translate")),
        choice(
          branch("function", functionReference),
          branch("type", typeReference),
          capture("module", token("module"))
        ),
        sequence(
          token("in"),
          capture("language", token("Id"))
        ).permissive,
        choice(
          sequence(
            token("="),
            branch("value", expression),
            token(";").permissive
          ),
          branch("definition", block)
        ).permissive
      )
    }

    val moduleDefinition = name("module definition") {
      sequence(
        capture("declaration", token("module")),
        branch("definition", block)
      )
    }
  }.syntax

  private class Operators(val rule: ExpressionRule) {
    def defineInfix(operator: String,
                    function: String,
                    precedence: Int,
                    right: Boolean = false) {
      val leftBindingPower = Int.MaxValue - precedence * 10
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

    private def argument(value: Node) = "argument" -> Node(
      "argument",
      value.getBegin,
      value.getEnd,
      List("value" -> value)
    )

    private def bindingPower(precedence: Int) = Int.MaxValue - precedence * 10
  }
}
