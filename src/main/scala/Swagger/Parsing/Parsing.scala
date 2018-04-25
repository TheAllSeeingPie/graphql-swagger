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

  def schema = phrase(rep1(blocks))

  def blocks = {
    val header = swagger ~ INFO ~
      INDENT ~ version ~ literal ~ description ~ literal ~ literal ~
      INDENT ~ literal ~
      DEDENT ~ literal ~
      INDENT ~ literal ~ literal ~
      DEDENT ~
      DEDENT ~ literal ~
      INDENT ~ choice ~
      DEDENT ^^ { _ => Ignored }
    val paths = PATHS ~
      INDENT ~ rep1(pathDef) ~ DEDENT ^^ { case _ ~ paths ~ _ => Paths(paths) }
    val definitions = DEFINITIONS ~
      INDENT ~ rep1(definition) ~ DEDENT ^^ { case _ ~ _ ~ definitions ~ _ => Definitions(definitions) }
    header | paths | definitions
  }

  //Only support GET for now
  def method = Swagger.Lexing.GET ^^ { _ => "GET" } //| PUT | POST | PATCH | DELETE | OPTIONS

  def pathDef = path ~
    INDENT ~ method ~
    INDENT ~ PRODUCES ~
    INDENT ~ choice ~
    DEDENT ~ RESPONSES ~
    INDENT ~ literal ~
    INDENT ~ description ~ SCHEMA ~
    INDENT ~ reference ~
    DEDENT ~ DEDENT ~ DEDENT ~ DEDENT ~ DEDENT ^^ { case path ~ _ ~ method ~ _ ~ _ ~ _ ~ _ ~ _ ~ _ ~ _ ~ _ ~ _ ~ _ ~ _ ~ _ ~ ref ~ _ ~ _ ~ _ ~ _ ~ _ => Path(path, method, ref) }

  def definition = literal ~
    INDENT ~ objectTypeDef ~ PROPERTIES ~ INDENT ~ rep1(property) ~
    DEDENT ~ DEDENT ^^ { case name ~ _ ~ objectType ~ _ ~ _ ~ properties ~ _ ~ _ => TypeDef(name, objectType, properties) }

  def property = literal ~
    INDENT ~ propertyTypeDef ~ ENUM ~
    INDENT ~ choice ~
    DEDENT ~ DEDENT ^^ { case name ~ _ ~ propertyType ~ _ ~ _ ~ choice ~ _ ~ _ => Property(name, propertyType) }

  def literal = accept("literal", { case LITERAL(n, _) => n })

  def swagger = accept("swagger", { case SWAGGER(v) => v })

  def version = accept("version", { case VERSION(v) => v })

  def description = accept("description", { case DESCRIPTION(d) => d })

  def choice = accept("choice", { case CHOICE(v) => v })

  def path = accept("path", { case PATH(p) => p })

  def reference = accept("reference", { case REFERENCE(r) => r.substring(14) })

  def objectTypeDef = accept("objectTypeDef", {
    case TYPE(tn) => tn match {
      case "object" => ObjectType
    }
  })

  def propertyTypeDef = accept("propertyTypeDef", {
    case TYPE(tn) => tn match {
      case "string" => StringType
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

trait SwaggerSchemaStatusCode

case object OK extends SwaggerSchemaStatusCode
