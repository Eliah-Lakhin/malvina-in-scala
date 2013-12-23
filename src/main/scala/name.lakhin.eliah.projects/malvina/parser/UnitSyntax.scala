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

import name.lakhin.eliah.projects.papacarlo.Syntax
import name.lakhin.eliah.projects.papacarlo.syntax.{Node, Rule}
import name.lakhin.eliah.projects.papacarlo.syntax.rules.NamedRule

object UnitSyntax {
  def apply(syntax: Syntax) {
    new {
      import syntax._
      import Rule._

      rule("compilation unit").main {
        sequence(
          zeroOrMore(branch("import", importDeclaration)),
          oneOrMore(branch("declaration", choice(functionDeclaration.debug,
            typeDeclaration.debug, static, metafunction)))
        )
      }

      val importDeclaration = rule("import") {
        sequence(
          token("import"),
          capture("module", token("module")),
          token(";").permissive
        )
      }

      val functionDeclaration = rule("function declaration") {
        sequence(
          optional(capture("export", token("export"))),
          token("#"),
          capture("name", token("id")),
          token("("),
          zeroOrMore(branch("parameter", parameter), token(",")),
          token(")").permissive,
          optional(sequence(token(":"), branch("result", typeApplication))),
          choice(branch("body", block), token(";").permissive)
        )
      }

      val typeDeclaration = rule("type declaration") {
        sequence(
          optional(capture("export", token("export"))),
          capture("name", token("Id")),
          optional(branch("pattern", typePattern)),
          choice(
            sequence(
              token("("),
              zeroOrMore(branch("parameter", parameter), token(",")),
              token(")").permissive,
              branch("body", block)
            ),
            token(";").permissive
          )
        )
      }

      val static = rule("static") {
        sequence(
          optional(capture("export", token("export"))),
          capture("name", token("id")),
          sequence(token(":"), branch("result", typeApplication)).permissive,
          sequence(token("="), branch("value", expression)).permissive,
          token(";").permissive
        )
      }

      val metafunction = rule("metafunction") {
        sequence(
          optional(capture("export", token("export"))),
          token("$"),
          capture("language", token("Id")),
          optional(capture("language-module", token("module"))),
          choice(
            branch("function", functionReference),
            branch("type", typeReference)
          ),
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

      val functionReference = rule("function reference") {
        sequence(
          token("#"),
          capture("name", token("id")),
          optional(capture("module", token("module"))),
          optional(sequence(
            capture("types", token("(")),
            zeroOrMore(branch("type", typeReference), token(",")),
            token(")").permissive
          )),
          optional(sequence(token(":"), branch("result", typeReference)))
        )
      }

      val typeReference = rule("type reference") {
        sequence(
          capture("name", token("Id")),
          optional(capture("module", token("module"))),
          optional(sequence(
            token("<"),
            capture("parameters", token("decimal")),
            token(">").permissive
          ))
        )
      }

      val typePattern = rule("type pattern") {
        sequence(
          token("<"),
          oneOrMore(capture("variable", token("type variable")), token(",")),
          token(">").permissive
        )
      }

      val typeApplication: NamedRule = rule("type application") {
        new TypeSyntax(
          Rule.expression(branch("operand", canonicalTypeApplication))
        ).rule
      }

      val canonicalTypeApplication =
        rule("canonical type application").produce("type application") {
          sequence(
            capture("name", choice(token("Id"), token("type variable"))),
            optional(capture("module", token("module"))),
            optional(sequence(
              token("<"),
              oneOrMore(
                branch("parameter", typeApplication.permissive),
                separator = token(",")
              ),
              token(">").permissive
            ))
          )
        }

      val expression: NamedRule = rule("expression") {
        new ExpressionSyntax(
          Rule.expression(branch("operand",
            operand.permissive("operand required")))
        ).rule
      }

      val array = rule("array").transform {
        node =>
          node.getBranches("element") match {
            case Nil =>
              node
                .accessor
                .setKind("application")
                .setConstant("name", "emptyArray")
                .node

            case head :: rest =>
              rest
                .foldLeft(Node("application", head.getBegin, head.getEnd,
                  branches = List("argument" -> head),
                  constants = Map("name" -> "arrayOf"))) {
                  (result, next) =>
                    Node(
                      "application",
                      result.getBegin,
                      next.getEnd,
                      branches = List(
                        "argument" -> result,
                        "argument" -> next
                      ),
                      constants = Map("name" -> "set")
                    )
                }
                .accessor
                .setBegin(node.getBegin)
                .setEnd(node.getEnd)
                .node
          }
      } {
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

      val application = rule("application") {
        sequence(
          capture("name", token("id")),
          optional(capture("module", token("module"))),
          optional(sequence(
            token("<"),
            oneOrMore(branch("parameter", typeApplication), token(",")),
            token(">")
          )),
          token("("),
          zeroOrMore(branch("argument", choice(argument, expression)).required,
            token(",")),
          token(")").permissive
        )
      }

      val argument = rule("argument") {
        sequence(
          capture("name", token("id")),
          token(":"),
          branch("value", expression)
        )
      }

      val variable = rule("variable").produce("application") {
        capture("name", token("id"))
      }

      val instantiation = rule("instantiation").transform {
        node =>
          node.accessor.setConstant("name", "new" + node.getValue("name"))

          node
      } {
        sequence(
          capture("name", token("Id")),
          optional(capture("module", token("module"))),
          token("("),
          zeroOrMore(branch("argument", choice(argument, expression)).required,
            token(",")),
          token(")").permissive
        )
      }

      val function = rule("function") {
        sequence(
          token("#"),
          choice(
            sequence(
              token("("),
              zeroOrMore(branch("parameter", parameter), token(",")),
              token("=>"),
              branch("result", typeApplication),
              token(")").permissive,
              branch("body", block).permissive
            ),
            sequence(
              oneOrMore(branch("parameter", parameter), token(",")),
              token("=>"),
              choice(
                branch("body", expression).permissive,
                branch("body", block).permissive
              )
            ),
            branch("body", block).permissive
          )
        )
      }

      val parameter = rule("parameter") {
        sequence(
          capture("name", token("id")),
          optional(sequence(
            token(":"),
            capture("type", typeApplication)
          )),
          optional(sequence(
            token("="),
            branch("value", expression)
          ))
        )
      }

      val expressionStatement = rule("expression statement") {
        sequence(
          branch("expression", expression),
          token(";").permissive
        ).required
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

      val binaryBranching: NamedRule = rule("if") {
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
           branch("condition", choice(decomposition, expression)),
           separator = token(",")
         )
      }

      val decomposition = rule("decomposition") {
        sequence(
          capture("name", token("id")),
          token(":"),
          optional(capture("type", typeApplication)),
          token("="),
          branch("value", expression)
        )
      }

      val multipleBranching: NamedRule = rule("switch").transform {
        node =>
          node.getBranches("conditions").zip(node.getBranches("body"))
            .foldRight(node.getBranch("default").getOrElse(Node("block",
              node.getEnd, node.getEnd))) {
            (pair, result) =>
              Node("if", pair._1.getBegin, pair._2.getEnd, branches = List(
                "conditions" -> pair._1,
                "success" -> pair._2,
                "fail" -> result
              ))
          }
            .accessor
            .setBegin(node.getBegin)
            .setEnd(node.getEnd)
            .node
      } {
        sequence(
          token("if"),
          token("{").permissive,
          oneOrMore(sequence(
            branch("conditions", conditions),
            token(":"),
            branch("body", bodyStatement)
          )),
          optional(sequence(
            token("else"),
            token(":"),
            branch("default", bodyStatement)
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
          optional(branch("value", expression)),
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
          sequence(token("="), branch("value", expression)).permissive,
          token(";").permissive
        )
      }

      val operand = subrule("operand") {
        choice(array, integer, float, string, nullLiteral, booleanLiteral,
          application, function, variable, instantiation)
      }

      val bodyStatement = rule("body statement").transform {
        node =>
          if (node.getKind == "block") node
          else Node("block", node.getBegin, node.getEnd,
            branches = List("statement" -> node))
      } {
        choice(block, binaryBranching, multipleBranching, loop, returnStatement,
          breakStatement, expressionStatement)
      }
    }
  }
}
