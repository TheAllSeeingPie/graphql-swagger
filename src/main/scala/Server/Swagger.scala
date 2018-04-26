package Server

import Swagger.Lexing.SwaggerSchemaLexer
import Swagger.Parsing._
import com.twitter.finagle.Http
import com.twitter.finagle.http.{Method, Request}
import com.twitter.util.Await
import org.json4s.{DefaultFormats, JValue}
import sangria.schema.{Field, ObjectType, Schema, StringType, fields}
import org.json4s.jackson.JsonMethods._

object SwaggerSchemaDefinition {
  def apply(swaggerUri: String): SwaggerSchemaDefinition = new SwaggerSchemaDefinition(swaggerUri)
}

class SwaggerSchemaDefinition(swaggerUri: String) {

  val endpoint = Http.client.newService(swaggerUri)
  private val swaggerData = Await.result(endpoint(Request("/swagger")).map(r => r.contentString))
  private val Right(swaggerLexed) = SwaggerSchemaLexer(swaggerData)
  private val Right(swagger) = SwaggerSchemaParser(swaggerLexed)

  private val QueryType = ObjectType("Query", "The root of all queries", fields(getFields(swagger): _*))
  val schema = Schema(QueryType)

  private def getFields(swagger: List[SwaggerSchemaAST]) = {
    swagger match {
      case Ignored :: Paths(paths) :: Definitions(types) :: Nil =>
        paths.map(p => {
          implicit val formats = DefaultFormats
          val typeDef = types.filter(t => t.name == p.reference).head
          typeDef.`type` match {
            case Swagger.Parsing.ObjectType => {
              val o = ObjectType(typeDef.name, typeDef.properties.map(p => {
                val f: Field[Unit, JValue] = Field(p.name, StringType, resolve = ctx => (ctx.value \ p.name).extract[String])
                f
              }))
              val f: Field[Unit, JValue] = Field(typeDef.name, o, resolve = _ => {
                val request = Request(Method(p.method), p.path)
                val response = endpoint(request)
                Await.result(response.map(r => parse(r.contentString)))
              })
              f
            }
          }
        })
    }
  }
}



