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
        oneOrMore(branch("import", importDeclaration))
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
          optional(capture("module", token("module"))),
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
          optional(capture("module", token("module"))),
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

      val thisReference = rule("this") {token("this")}

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
          capture("name", choice(token("id"), token("this"))),
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
          token("new"),
          capture("name", token("Id")),
          optional(capture("module", token("module"))),
          token("("),
          zeroOrMore(branch("argument", choice(argument, expression)).required,
            token(",")),
          token(")").permissive
        )
      }

      val staticAccess = rule("static").transform {
        node =>
          node.accessor
            .setKind("application")
            .setConstant("name", "instanceOf" + node.getValue("name"))

          node
      } {
        capture("name", token("Id"))
      }

      val function = rule("function") {
        sequence(
          token("#"),
          choice(
            sequence(
              token("("),
              zeroOrMore(branch("parameter", functionParameter), token(",")),
              token("=>"),
              branch("result", typeApplication),
              token(")").permissive,
              branch("body", block).permissive
            ),
            sequence(
              oneOrMore(branch("parameter", functionParameter), token(",")),
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

      val functionParameter = rule("function parameter") {
        sequence(
          capture("name", token("id")),
          optional(sequence(
            token(":"),
            capture("type", typeApplication)
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
          thisReference, application, function, variable, instantiation,
          staticAccess)
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

      val importDeclaration = subrule("import declaration") {
        sequence(
          token("import"),
          capture("module", token("module")),
          token(";").permissive
        )
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
    }
  }
}
