package Server

import Swagger.Lexing._
import org.scalatest.Matchers

class SwaggerSchemaLexerTest extends org.scalatest.FlatSpec with Matchers {
  val helloWorld = "swagger: '2.0'\ninfo:\n  version: 0.0.0\n  title: title\n  description: description\n  termsOfService: terms\n  contact:\n    name: TheAllSeeingPie\n  license:\n    name: MIT\n    url: http://opensource.org/licenses/MIT\nschemes:\n  - http\npaths:\n  /:\n    get:\n      produces: \n        - \"application/json\"\n      responses:\n        200:\n          description: \"Just a simple response\"\n          schema:\n            $ref: \"#/definitions/HelloWorld\"\ndefinitions:\n  HelloWorld:\n    type: \"object\"\n    properties:\n      hello:\n        type: \"string\"\n        enum:\n          - \"world!\""
  val Right(result) = SwaggerSchemaLexer(helloWorld)
  println(result)

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
