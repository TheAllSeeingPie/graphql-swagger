package Server

import com.twitter.finagle.http.Request
import com.twitter.util.Await
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.scalatest.Matchers

class GraphQLServiceSpec extends org.scalatest.FlatSpec with Matchers {
  "GraphQLService" should "return \"world!\"" in {
    val request = Request("/graphql")
    val json = ("query" -> "{ hello }") ~ ("variables" -> JNull)
    request.contentString = compact(render(json))
    val response = Await.result(GraphQLService(PersonSchemaDefinition)(request))
    response.contentString should be("{\"data\":{\"hello\":\"world!\"}}")
  }

  "GraphQLService" should "return \"person\"" in {
    val request = Request("/graphql")
    val json = ("query" -> "{ person(id: 1) { name } }") ~ ("variables" -> JNull)
    request.contentString = compact(render(json))
    val response = Await.result(GraphQLService(PersonSchemaDefinition)(request))
    response.contentString should be("{\"data\":{\"person\":{\"name\":\"Bob\"}}}")
  }

  "GraphQLService" should "return \"person\" from variables" in {
    val request = Request("/graphql")
    val json = ("query" -> "query personQ($id: BigInt!) { person(id: $id) { name } }") ~ ("variables" -> ("id" -> 1)) ~ ("operationName" -> "personQ")
    request.contentString = compact(render(json))
    val response = Await.result(GraphQLService(PersonSchemaDefinition)(request))
    response.contentString should be("{\"data\":{\"person\":{\"name\":\"Bob\"}}}")
  }

  "parsing empty vars" should "produce JObject(JList))" in {
    val json = compact(render("variables" -> JNull))
    val parsed = (parse(json) \\ "variables").toOption
    parsed should be(None)
  }
}
