package Server

import Swagger.Lexing._
import org.scalatest.Matchers

import scala.io.Source

class SwaggerSchemaLexerSpec extends org.scalatest.FlatSpec with Matchers {
  val helloWorld = Source.fromInputStream(getClass().getClassLoader().getResourceAsStream("HelloWorld.swagger")).mkString
  val Right(result) = SwaggerSchemaLexer(helloWorld)

  behavior of "SwaggerSchemaLexer"

  private def verify(schema: SwaggerSchema, amount: Int = 1) = {
    val expected = (0 until amount).map(_ => schema).toList
    result.filter(_ == schema) should be(expected)
  }

  it should "be able to get at the path: /" in {
    verify(PATH("/"))
  }

  it should "be able to get the verb: GET" in {
    verify(GET)
  }

  it should "be able to get the reference to definition" in {
    verify(REFERENCE("#/definitions/HelloWorld"))
  }
}
