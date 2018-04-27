package Swagger.Parsing

import Swagger.Lexing.{INDENT, _}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Reader}

object SwaggerSchemaParser extends Parsers {
  override type Elem = SwaggerSchema

  def apply(tokens: Seq[SwaggerSchema]) = {
    val reader = new SwaggerSchemaReader(tokens)
    schema(reader) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _) => Left(SwaggerSchemaParserError(msg))
    }
  }

  private def schema = phrase(rep1(blocks))

  private def blocks = {
    val header = swagger ^^ { _ => Ignored }
    val info = INFO ~
      INDENT ~ rep1(version | literal | description | termsOfService) ~
      INDENT ~ rep1(literal) ~
      DEDENT ~
      literal ~
      INDENT ~ rep1(literal) ~
      DEDENT ~
      DEDENT ^^ { _ => Ignored }
    val host = parseHost ^^ { _ => Ignored }
    val basePath = parseBasePath ^^ { _ => Ignored }
    val availableSchemes = SCHEMES ~
      INDENT ~ choice ~
      DEDENT ^^ { _ => Ignored }
    val parseExternalDocs = EXTERNALDOCS ~ INDENT ~ rep1(description | literal) ~ DEDENT
    val tags = TAGS ~
      INDENT ~ rep1(literal ~
      INDENT ~ rep1(description | parseExternalDocs) ~ DEDENT) ~
      DEDENT ^^ { _ => Ignored }
    val externalDocs = parseExternalDocs ^^ { _ => Ignored }
    val securityDefinitions = SECURITYDEFINITIONS ^^ { _ => Ignored }
    val paths = PATHS ~
      INDENT ~ rep1(pathDef) ~ DEDENT ^^ { case _ ~ paths ~ _ => Paths(paths) }
    val definitions = DEFINITIONS ~
      INDENT ~ rep1(definition) ~ DEDENT ^^ { case _ ~ _ ~ definitions ~ _ => Definitions(definitions) }
    header | info | host | basePath | availableSchemes |
      tags | externalDocs | securityDefinitions | paths | definitions
  }

  //Only support GET for now
  private def method = Swagger.Lexing.GET ^^ { _ => "GET" } //| PUT | POST | PATCH | DELETE | OPTIONS

  private def pathDef = path ~
    INDENT ~ method ~
    INDENT ~ PRODUCES ~
    INDENT ~ choice ~
    DEDENT ~ RESPONSES ~
    INDENT ~ literal ~
    INDENT ~ description ~ SCHEMA ~
    INDENT ~ reference ~
    DEDENT ~ DEDENT ~ DEDENT ~ DEDENT ~ DEDENT ^^ { case path ~ _ ~ method ~ _ ~ _ ~ _ ~ _ ~ _ ~ _ ~ _ ~ _ ~ _ ~ _ ~ _ ~ _ ~ ref ~ _ ~ _ ~ _ ~ _ ~ _ => Path(path, method, ref) }

  private def definition = literal ~
    INDENT ~ objectTypeDef ~ PROPERTIES ~ INDENT ~ rep1(property) ~
    DEDENT ~ DEDENT ^^ { case name ~ _ ~ objectType ~ _ ~ _ ~ properties ~ _ ~ _ => TypeDef(name, objectType, properties) }

  private def property = {
    val short = literal ~
      INDENT ~ propertyTypeDef ~ DEDENT ^^ { case name ~ _ ~ propertyType ~ _ => Property(name, propertyType) }

    val enumed = literal ~
      INDENT ~ propertyTypeDef ~ ENUM ~
      INDENT ~ choice ~
      DEDENT ~ DEDENT ^^ { case name ~ _ ~ propertyType ~ _ ~ _ ~ _ ~ _ ~ _ => Property(name, propertyType) }

    enumed | short
  }

  private def literal = accept("literal", { case LITERAL(n, _) => n })

  private def swagger = accept("swagger", { case SWAGGER(v) => v })

  private def parseHost = accept("parseHost", { case HOST(h) => h })

  private def parseBasePath = accept("parseBasePath", { case BASEPATH(h) => h })

  private def version = accept("version", { case VERSION(v) => v })

  private def description = accept("description", { case DESCRIPTION(d) => d })

  private def termsOfService = accept("termsOfService", { case TERMSOFSERVICE(t) => t })

  private def choice = accept("choice", { case CHOICE(v) => v })

  private def path = accept("path", { case PATH(p) => p })

  private def reference = accept("reference", { case REFERENCE(r) => r.substring(14) })

  private def objectTypeDef = accept("objectTypeDef", {
    case TYPE(tn) => tn match {
      case "object" => ObjectType
    }
  })

  private def propertyTypeDef = accept("propertyTypeDef", {
    case TYPE(tn) => tn match {
      case "string" => StringType
      case "integer" => IntegerType
    }
  })

}

class SwaggerSchemaReader(tokens: Seq[SwaggerSchema]) extends Reader[SwaggerSchema] {
  override def first = tokens.head

  override def atEnd = tokens.isEmpty

  override def pos = NoPosition

  override def rest = new SwaggerSchemaReader(tokens.tail)
}

sealed trait SwaggerSchemaAST

case object Ignored extends SwaggerSchemaAST

case class Definitions(types: List[TypeDef]) extends SwaggerSchemaAST

case class TypeDef(name: String, `type`: SwaggerSchemaObjectType, properties: List[Property]) extends SwaggerSchemaAST

case class TypeName(name: String) extends SwaggerSchemaAST

case class Property(name: String, `type`: SwaggerSchemaPropertyType) extends SwaggerSchemaAST

case class Paths(paths: List[Path]) extends SwaggerSchemaAST

case class Path(path: String, method: String, reference: String) extends SwaggerSchemaAST

trait SwaggerSchemaObjectType

case object ObjectType extends SwaggerSchemaObjectType

trait SwaggerSchemaPropertyType

case object StringType extends SwaggerSchemaPropertyType

case object IntegerType extends SwaggerSchemaPropertyType

trait SwaggerSchemaStatusCode

case object OK extends SwaggerSchemaStatusCode
