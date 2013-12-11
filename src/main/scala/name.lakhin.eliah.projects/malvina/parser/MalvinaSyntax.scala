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

import name.lakhin.eliah.projects.papacarlo.{Lexer, Syntax}
import name.lakhin.eliah.projects.papacarlo.syntax.{Node, Rule}
import name.lakhin.eliah.projects.papacarlo.syntax.rules.NamedRule

object MalvinaSyntax {
  def apply(lexer: Lexer) = new {
    val syntax = new Syntax(lexer)

    import syntax._
    import Rule._

    rule("compilation unit").main {
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
        branch("body", constructorBody.debug)
      )
    }

    val constructorBody = rule("constructor body").cachable {
      sequence(
        token("{"),
        zeroOrMore(branch("statement", choice(propertyDefinition,
          binaryBranching, multipleBranching, loop, returnStatement,
          breakStatement, variableDefinition, expressionStatement))),
        token("}").permissive
      )
    }

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
        capture("name", choice(token("id"), token("this"))),
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
      branch("operand", operand.permissive("operand required"))
    }

    val array = rule("array") {
      sequence(
        token("["),
        zeroOrMore(branch("element", expression), token(",")),
        token("]").permissive
      )
    }

    val integer = rule("integer").transform {
      _.accessor
        .setKind("application")
        .setConstant("name", "integer")
        .node
    } {
      capture("value", choice(token("decimal"), token("hex")))
    }

    val float = rule("float").transform {
      _.accessor
        .setKind("application")
        .setConstant("name", "float")
        .node
    } {
      capture("value", token("float"))
    }

    val string = rule("string").transform {
      _.accessor
        .setKind("application")
        .setConstant("name", "string")
        .node
    } {
      capture("value", token("string"))
    }

    val nullLiteral = rule("null").transform {
      _.accessor
        .setKind("application")
        .setConstant("name", "nullable")
        .node
    } {
      capture("name", token("null"))
    }

    val booleanLiteral = rule("boolean").transform {
      _.accessor
        .setKind("application")
        .setConstant("name", "boolean")
        .node
    } {
      capture("name", choice(token("true"), token("false")))
    }

    val thisReference = rule("this") {token("this")}

    val application = rule("application") {
      sequence(
        capture("name", token("id")),
        optional(sequence(
          token("@"),
          capture("module", token("id")).permissive
        )),
        token("("),
        zeroOrMore(branch("argument", argument), token(",")),
        token(")").permissive
      )
    }

    val argument = rule("argument") {
      sequence(
        optional(sequence(capture("name", token("id")), token(":"))),
        branch("value", expression).required
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

    val singleParameter = rule("single parameter").transform {
      single =>
        Node(
          "function parameters", single.getBegin, single.getEnd,
          branches = List("parameter" ->
            single.accessor.setKind("function parameter").node)
        )
    } {
      capture("name", token("id"))
    }

    val expressionBody = rule("expression body").transform {
      expression =>
        Node("block", expression.getBegin, expression.getEnd, branches = List(
          "statement" -> expression.accessor.setKind("expression statement")
            .node
        ))
    } {
      branch("expression", expression)
    }

    val block: NamedRule = rule("block").cachable {
      sequence(
        token("{"),
        zeroOrMore(branch("statement", choice(binaryBranching,
          multipleBranching, loop, returnStatement, breakStatement,
          variableDefinition, expressionStatement))),
        token("}").permissive
      )
    }

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
        capture("name", token("id")),
        token(":"),
        optional(capture("type", typeApplication)),
        sequence(
          token("="),
          branch("value", expression)
        ).permissive,
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

    val operand = subrule("operand") {
      choice(array, integer, float, string, nullLiteral, booleanLiteral,
        thisReference, application, function, variable)
    }

    val bodyStatement = subrule("body statement") {
      choice(block, binaryBranching, multipleBranching, loop, returnStatement,
        breakStatement, expressionStatement)
    }

    val moduleReference = subrule("module reference") {
      sequence(token("@"), capture("module", token("id")))
    }

    val functionDeclaration = subrule("function declaration") {
      sequence(
        capture("declaration", token("function")),
        capture("name", token("id")),
        branch("parameters", methodParameters),
        optional(sequence(token(":"), branch("result", typeApplication))),
        choice(branch("body", block), token(";").permissive)
      )
    }

    val typeDeclaration = subrule("type declaration") {
      sequence(
        capture("declaration", token("type")),
        capture("name", token("Id")),
        optional(branch("pattern", typePattern)),
        choice(branch("constructor", constructor), token(";").permissive)
      )
    }

    val staticDeclaration = subrule("static declaration") {
      sequence(
        capture("declaration", token("static")),
        capture("name", token("Id")),
        sequence(token(":"), branch("result", typeApplication)).permissive,
        sequence(token("="), branch("value", expression)).permissive,
        token(";").permissive
      )
    }

    val translator = subrule("translator") {
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

    val moduleDefinition = subrule("module definition") {
      sequence(
        capture("declaration", token("module")),
        branch("definition", block)
      )
    }
  }.syntax
}
