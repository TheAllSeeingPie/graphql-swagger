package Server

import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.{Http, Service}
import com.twitter.util.{Await, Future}
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods.{compact, render}

import scala.io.Source

object Server extends App {
  override def main(args: Array[String]) = {
    val testApi = Http.serve(":8000", new Service[Request, Response] {
      override def apply(request: Request) = if (request.path == "/swagger") Future.value({
        val r = Response()
        r.contentString = Source.fromInputStream(getClass().getClassLoader().getResourceAsStream("HelloWorld.swagger")).mkString
        r
      }) else Future.value({
        val r = Response()
        r.contentString = Source.fromInputStream(getClass().getClassLoader().getResourceAsStream("HelloWorld.json")).mkString
        r
      })
    })
    val schema = SwaggerSchemaDefinition(":8000")
    val dynamicExample = Http.serve(":3000", new GraphQLService(schema.schema))
    Await.all(testApi, dynamicExample)
  }
}


