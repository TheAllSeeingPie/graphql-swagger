package Server

import Swagger.Lexing.SwaggerSchemaLexer
import Swagger.Parsing.{Definitions, Ignored, SwaggerSchemaAST, SwaggerSchemaParser}
import com.twitter.finagle.Http
import com.twitter.finagle.http.Request
import com.twitter.util.Await
import sangria.schema.{Field, ObjectType, Schema, StringType, fields}

object SwaggerSchemaDefinition {
  def apply(swaggerUri: String): SwaggerSchemaDefinition = new SwaggerSchemaDefinition(swaggerUri)
}

class SwaggerSchemaDefinition(swaggerUri: String) extends SchemaDefinition {

  val endpoint = Http.client.newService(swaggerUri)
  val swaggerData = Await.result(endpoint(Request("/swagger")).map(r => r.contentString))
  val Right(swaggerLexed) = SwaggerSchemaLexer(swaggerData)
  val Right(swagger) = SwaggerSchemaParser(swaggerLexed)

  val QueryType = ObjectType("Query", "The root of all queries", fields(getFields(swagger): _*))
  override val schema = Schema(QueryType)

  def getFields(swagger: List[SwaggerSchemaAST]): List[Field[Unit, Unit]] = swagger match {
    case Ignored :: Ignored :: Definitions(types) :: Nil => types.map(t => {
      t.`type` match {
        case Swagger.Parsing.ObjectType => {
          val o = ObjectType(t.name, t.properties.map(p => {
            val f: Field[Unit, Unit] = Field(p.name, StringType, resolve = _ => "woooo!")
            f
          }))
          val f : Field[Unit, Unit] = Field(t.name, o, resolve = _ => ())
          f
        }
      }
    })
  }
}



