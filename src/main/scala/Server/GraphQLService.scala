package Server

import com.twitter.bijection.Conversion.asMethod
import com.twitter.bijection.twitter_util.UtilBijections._
import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.util.{Future, FuturePool}
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods.{compact, render}
import sangria.execution.ExecutionScheme.Default
import sangria.execution._
import sangria.marshalling.json4s.Json4sJacksonSupportLowPrioImplicits
import sangria.marshalling.json4s.jackson.Json4sJacksonInputParser._
import sangria.marshalling.json4s.jackson.Json4sJacksonResultMarshaller.renderCompact
import sangria.marshalling.json4s.jackson._
import sangria.parser.QueryParser
import sangria.schema.Schema

import scala.io.Source
import scala.util.{Failure, Success}

trait SchemaDefinition {
  def schema : Schema[Unit, Unit]
}

object GraphQLService {
  def apply(schemaDefinition: SchemaDefinition): GraphQLService = new GraphQLService(schemaDefinition)
}

class GraphQLService(schemaDefinition: SchemaDefinition) extends Service[Request, Response] with Json4sJacksonSupportLowPrioImplicits {
  val pool = FuturePool.interruptibleUnboundedPool

  override def apply(request: Request) = {
    if (request.path == "/graphql") executeQuery(request)
    else pool(graphiql)
  }

  private def createResponse(content: String, status: Status = Status.Ok) = {
    val response = Response(status)
    response.contentString = content
    response
  }

  private val graphiql = createResponse(Source.fromInputStream(getClass().getClassLoader().getResourceAsStream("graphiql.html")).mkString)

  def executeQuery(request: Request) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    implicit val formats = org.json4s.DefaultFormats

    parse(request.contentString) match {
      case Success(input) => {
        val JString(query) = input \\ "query"
        val vars = (input \\ "variables").toOption
        val operation = (input \\ "operationName").extractOpt[String]
        QueryParser.parse(query) match {
          case Success(query) => {
            Executor.execute(schemaDefinition.schema, query, variables = vars.getOrElse(JObject(List())), operationName = operation)
              .as[Future[JValue]]
              .map(c => createResponse(renderCompact(c)))
              .rescue {
                case error: QueryAnalysisError => pool(createResponse(compact(render(error.resolveError)), Status.BadRequest))
                case error: ErrorWithResolver => pool(createResponse(compact(render(error.resolveError)), Status.InternalServerError))
              }
          }
          case Failure(failure) => pool(createResponse(compact(render("error" -> failure.getMessage)), Status.BadRequest))
        }
      }
      case Failure(failure) => pool(createResponse(failure.toString, Status.BadRequest))
    }
  }
}
