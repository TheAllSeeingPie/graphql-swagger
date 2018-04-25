package Server

import Swagger.Lexing.SwaggerSchemaLexer
import Swagger.Parsing.SwaggerSchemaParser
import org.scalatest.Matchers

class SwaggerSchemaParserTest extends org.scalatest.FlatSpec with Matchers {
  val helloWorld = "swagger: '2.0'\ninfo:\n  version: 0.0.0\n  title: title\n  description: description\n  termsOfService: terms\n  contact:\n    name: TheAllSeeingPie\n  license:\n    name: MIT\n    url: http://opensource.org/licenses/MIT\nschemes:\n  - http\npaths:\n  /:\n    get:\n      produces: \n        - \"application/json\"\n      responses:\n        200:\n          description: \"Just a simple response\"\n          schema:\n            $ref: \"#/definitions/HelloWorld\"\ndefinitions:\n  HelloWorld:\n    type: \"object\"\n    properties:\n      hello:\n        type: \"string\"\n        enum:\n          - \"world!\""

  val Right(result) = SwaggerSchemaLexer(helloWorld)
  val parsed = SwaggerSchemaParser(result)
  println(parsed)
  val Right(parsedResult) = parsed

  behavior of "SwaggerSchemaParser"

  it should "have some definitions" in {
  }
}
