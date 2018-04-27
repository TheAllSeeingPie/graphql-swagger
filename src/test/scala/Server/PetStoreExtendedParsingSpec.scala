package Server

import Swagger.Lexing.SwaggerSchemaLexer
import Swagger.Parsing.SwaggerSchemaParser
import org.scalatest.Matchers

import scala.io.Source

class PetStoreExtendedParsingSpec extends org.scalatest.FlatSpec with Matchers {
  val helloWorld = Source.fromInputStream(getClass().getClassLoader().getResourceAsStream("PetStore.swagger")).mkString

  val Right(result) = SwaggerSchemaLexer(helloWorld)
  println(result)
  val parsed = SwaggerSchemaParser(result)
  val Right(parsedResult) = parsed

  behavior of "SwaggerSchemaParser"

  it should "have some definitions" in {
  }
}
