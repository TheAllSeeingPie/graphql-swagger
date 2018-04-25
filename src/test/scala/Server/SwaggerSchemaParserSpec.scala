package Server

import Swagger.Lexing.SwaggerSchemaLexer
import Swagger.Parsing.SwaggerSchemaParser
import org.scalatest.Matchers

import scala.io.Source

class SwaggerSchemaParserSpec extends org.scalatest.FlatSpec with Matchers {
  val helloWorld = Source.fromInputStream(getClass().getClassLoader().getResourceAsStream("HelloWorld.swagger")).mkString

  val Right(result) = SwaggerSchemaLexer(helloWorld)
  val parsed = SwaggerSchemaParser(result)
  val Right(parsedResult) = parsed

  behavior of "SwaggerSchemaParser"

  it should "have some definitions" in {
  }
}
