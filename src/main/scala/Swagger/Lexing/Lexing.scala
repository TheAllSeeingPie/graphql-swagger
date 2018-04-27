package Swagger.Lexing

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

sealed trait SwaggerSchema

case class INDENTATION(spaces: Int) extends SwaggerSchema

case class LITERAL(name: String, value: Option[String] = None) extends SwaggerSchema

case class SWAGGER(version: String) extends SwaggerSchema

case object SCHEMES extends SwaggerSchema

case object SECURITYDEFINITIONS extends SwaggerSchema

case object TAGS extends SwaggerSchema

case object EXTERNALDOCS extends SwaggerSchema

case object INFO extends SwaggerSchema

case class VERSION(version: String) extends SwaggerSchema

case class TITLE(title: String) extends SwaggerSchema

case class CHOICE(value: String) extends SwaggerSchema

case object PATHS extends SwaggerSchema

case class PATH(path: String) extends SwaggerSchema

case object GET extends SwaggerSchema

case object PUT extends SwaggerSchema

case object POST extends SwaggerSchema

case object PATCH extends SwaggerSchema

case object DELETE extends SwaggerSchema

case object OPTIONS extends SwaggerSchema

case object PRODUCES extends SwaggerSchema

case object RESPONSES extends SwaggerSchema

case object DEFINITIONS extends SwaggerSchema

case class DESCRIPTION(description: String) extends SwaggerSchema

case object SCHEMA extends SwaggerSchema

case class REFERENCE(reference: String) extends SwaggerSchema

case class TYPE(`type`: String) extends SwaggerSchema

case object PROPERTIES extends SwaggerSchema

case object ENUM extends SwaggerSchema

case object INDENT extends SwaggerSchema

case object DEDENT extends SwaggerSchema

case class HOST(host: String) extends SwaggerSchema

case class TERMSOFSERVICE(host: String) extends SwaggerSchema

case class BASEPATH(path: String) extends SwaggerSchema

trait SwaggerSchemaError

case class SwaggerSchemaLexerError(msg: String) extends SwaggerSchemaError

case class SwaggerSchemaParserError(msg: String) extends SwaggerSchemaError

object SwaggerSchemaLexer extends RegexParsers {
  override def skipWhitespace = true

  override val whiteSpace = "[ \t\r\f]+".r

  def apply(schema: String) = {
    parse(tokens, schema) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _) => Left(SwaggerSchemaLexerError(msg))
    }
  }

  def processIndentations(tokens: List[SwaggerSchema], indents: List[Int] = List(0)): List[SwaggerSchema] = tokens.headOption match {
    case Some(INDENTATION(spaces)) if spaces > indents.head => INDENT :: processIndentations(tokens.tail, spaces :: indents)
    case Some(INDENTATION(spaces)) if spaces < indents.head =>
      val (d, k) = indents.partition(_ > spaces)
      d.map(_ => DEDENT) ::: processIndentations(tokens.tail, k)
    case Some(INDENTATION(spaces)) if spaces == indents.head => processIndentations(tokens.tail, indents)
    case Some(token) => token :: processIndentations(tokens.tail, indents)
    case None => indents.filter(_ > 0).map(_ => DEDENT)
  }

  private def tokens: Parser[List[SwaggerSchema]] = {
    phrase(rep1(indentation | paths | path | get | put | post | patch | delete | options | produces | responses
      | description | schema | reference | definitions | `type` | properties | enum
      | swagger | info | version | host | basePath | schemes | tags | externalDocs | termsOfService
      | securityDefinitions
      | literal | choice)) ^^ { t => processIndentations(t) }
  }

  private def indentation = "\n[ \t]*".r ^^ { whitespace => INDENTATION(whitespace.length - 1) }

  private def literal = {
    val expr = "(.+):[ ]?(.*)".r
    expr ^^ {
      s => {
        val result = expr.findFirstMatchIn(s).get
        result.group(2) match {
          case "" => LITERAL(result.group(1))
          case _ => LITERAL(result.group(1), Some(result.group(2)))
        }
      }
    }
  }

  private def termsOfService = pullFromExpr("termsOfService: [\"']?(.*)[\"']?".r, TERMSOFSERVICE(_))

  private def host = pullFromExpr("host: (.+)".r, HOST(_))

  private def basePath = pullFromExpr("basePath: (.+)".r, BASEPATH(_))

  private def schemes = "schemes:".r ^^ { _ => SCHEMES }

  private def tags = "tags:".r ^^ { _ => TAGS }

  private def externalDocs = "externalDocs:".r ^^ { _ => EXTERNALDOCS }

  private def securityDefinitions = "securityDefinitions:".r ^^ { _ => SECURITYDEFINITIONS }

  private def choice = pullFromExpr("-[ ](.*)".r, CHOICE(_))

  private def paths = "paths:".r ^^ { _ => PATHS }

  private def path = "\\/.*:".r ^^ { s => PATH(s.dropRight(1)) }

  private def get = "get:".r ^^ { _ => GET }

  private def put = "put:".r ^^ { _ => PUT }

  private def post = "post:".r ^^ { _ => POST }

  private def patch = "patch:".r ^^ { _ => PATCH }

  private def delete = "delete:".r ^^ { _ => DELETE }

  private def options = "options:".r ^^ { _ => OPTIONS }

  private def produces = "produces:".r ^^ { _ => PRODUCES }

  private def responses = "responses:".r ^^ { _ => RESPONSES }

  private def description = pullFromExpr("description: (.+)".r, DESCRIPTION(_))

  private def schema = "schema:".r ^^ { _ => SCHEMA }

  private def reference = pullFromExpr("\\$ref: (\"|')(.+)(\"|')".r, REFERENCE(_), 2)

  private def definitions = "definitions:".r ^^ { _ => DEFINITIONS }

  private def `type` = pullFromExpr("type: \"(.+)\"".r, TYPE(_))

  private def properties = "properties:".r ^^ { _ => PROPERTIES }

  private def enum = "enum:".r ^^ { _ => ENUM }

  private def info = "info:".r ^^ { _ => INFO }

  private def swagger = pullFromExpr("swagger: \'(.+)\'".r, SWAGGER(_))

  private def version = pullFromExpr("version: (.+)".r, VERSION(_))

  private def pullFromExpr(expr: Regex, f: String => SwaggerSchema, group: Int = 1) = {
    expr ^^ { s => f(expr.findFirstMatchIn(s).get.group(group)) }
  }
}
