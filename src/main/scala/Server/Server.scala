package Server

import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.{Http, Service}
import com.twitter.util.{Await, Future}
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods.{compact, render}

object Server extends App {
  override def main(args: Array[String]) = {
    val testApi = Http.serve(":8000", new Service[Request, Response] {
      override def apply(request: Request) = if (request.path == "/swagger") Future.value({
        val r = Response()
        //r.contentString = "swagger: \"2.0\"\ninfo:\n  description: \"This is a sample server Petstore server.  You can find out more about     Swagger at [http://swagger.io](http://swagger.io) or on [irc.freenode.net, #swagger](http://swagger.io/irc/).      For this sample, you can use the api key `special-key` to test the authorization     filters.\"\n  version: \"1.0.0\"\n  title: \"Swagger Petstore\"\n  termsOfService: \"http://swagger.io/terms/\"\n  contact:\n    email: \"apiteam@swagger.io\"\n  license:\n    name: \"Apache 2.0\"\n    url: \"http://www.apache.org/licenses/LICENSE-2.0.html\"\nhost: \"petstore.swagger.io\"\nbasePath: \"/v2\"\ntags:\n- name: \"pet\"\n  description: \"Everything about your Pets\"\n  externalDocs:\n    description: \"Find out more\"\n    url: \"http://swagger.io\"\n- name: \"store\"\n  description: \"Access to Petstore orders\"\n- name: \"user\"\n  description: \"Operations about user\"\n  externalDocs:\n    description: \"Find out more about our store\"\n    url: \"http://swagger.io\"\nschemes:\n- \"http\"\npaths:\n  /pet:\n    post:\n      tags:\n      - \"pet\"\n      summary: \"Add a new pet to the store\"\n      description: \"\"\n      operationId: \"addPet\"\n      consumes:\n      - \"application/json\"\n      - \"application/xml\"\n      produces:\n      - \"application/xml\"\n      - \"application/json\"\n      parameters:\n      - in: \"body\"\n        name: \"body\"\n        description: \"Pet object that needs to be added to the store\"\n        required: true\n        schema:\n          $ref: \"#/definitions/Pet\"\n      responses:\n        405:\n          description: \"Invalid input\"\n      security:\n      - petstore_auth:\n        - \"write:pets\"\n        - \"read:pets\"\n    put:\n      tags:\n      - \"pet\"\n      summary: \"Update an existing pet\"\n      description: \"\"\n      operationId: \"updatePet\"\n      consumes:\n      - \"application/json\"\n      - \"application/xml\"\n      produces:\n      - \"application/xml\"\n      - \"application/json\"\n      parameters:\n      - in: \"body\"\n        name: \"body\"\n        description: \"Pet object that needs to be added to the store\"\n        required: true\n        schema:\n          $ref: \"#/definitions/Pet\"\n      responses:\n        400:\n          description: \"Invalid ID supplied\"\n        404:\n          description: \"Pet not found\"\n        405:\n          description: \"Validation exception\"\n      security:\n      - petstore_auth:\n        - \"write:pets\"\n        - \"read:pets\"\n  /pet/findByStatus:\n    get:\n      tags:\n      - \"pet\"\n      summary: \"Finds Pets by status\"\n      description: \"Multiple status values can be provided with comma separated strings\"\n      operationId: \"findPetsByStatus\"\n      produces:\n      - \"application/xml\"\n      - \"application/json\"\n      parameters:\n      - name: \"status\"\n        in: \"query\"\n        description: \"Status values that need to be considered for filter\"\n        required: true\n        type: \"array\"\n        items:\n          type: \"string\"\n          enum:\n          - \"available\"\n          - \"pending\"\n          - \"sold\"\n          default: \"available\"\n        collectionFormat: \"multi\"\n      responses:\n        200:\n          description: \"successful operation\"\n          schema:\n            type: \"array\"\n            items:\n              $ref: \"#/definitions/Pet\"\n        400:\n          description: \"Invalid status value\"\n      security:\n      - petstore_auth:\n        - \"write:pets\"\n        - \"read:pets\"\n  /pet/findByTags:\n    get:\n      tags:\n      - \"pet\"\n      summary: \"Finds Pets by tags\"\n      description: \"Muliple tags can be provided with comma separated strings. Use         tag1, tag2, tag3 for testing.\"\n      operationId: \"findPetsByTags\"\n      produces:\n      - \"application/xml\"\n      - \"application/json\"\n      parameters:\n      - name: \"tags\"\n        in: \"query\"\n        description: \"Tags to filter by\"\n        required: true\n        type: \"array\"\n        items:\n          type: \"string\"\n        collectionFormat: \"multi\"\n      responses:\n        200:\n          description: \"successful operation\"\n          schema:\n            type: \"array\"\n            items:\n              $ref: \"#/definitions/Pet\"\n        400:\n          description: \"Invalid tag value\"\n      security:\n      - petstore_auth:\n        - \"write:pets\"\n        - \"read:pets\"\n      deprecated: true\n  /pet/{petId}:\n    get:\n      tags:\n      - \"pet\"\n      summary: \"Find pet by ID\"\n      description: \"Returns a single pet\"\n      operationId: \"getPetById\"\n      produces:\n      - \"application/xml\"\n      - \"application/json\"\n      parameters:\n      - name: \"petId\"\n        in: \"path\"\n        description: \"ID of pet to return\"\n        required: true\n        type: \"integer\"\n        format: \"int64\"\n      responses:\n        200:\n          description: \"successful operation\"\n          schema:\n            $ref: \"#/definitions/Pet\"\n        400:\n          description: \"Invalid ID supplied\"\n        404:\n          description: \"Pet not found\"\n      security:\n      - api_key: []\n    post:\n      tags:\n      - \"pet\"\n      summary: \"Updates a pet in the store with form data\"\n      description: \"\"\n      operationId: \"updatePetWithForm\"\n      consumes:\n      - \"application/x-www-form-urlencoded\"\n      produces:\n      - \"application/xml\"\n      - \"application/json\"\n      parameters:\n      - name: \"petId\"\n        in: \"path\"\n        description: \"ID of pet that needs to be updated\"\n        required: true\n        type: \"integer\"\n        format: \"int64\"\n      - name: \"name\"\n        in: \"formData\"\n        description: \"Updated name of the pet\"\n        required: false\n        type: \"string\"\n      - name: \"status\"\n        in: \"formData\"\n        description: \"Updated status of the pet\"\n        required: false\n        type: \"string\"\n      responses:\n        405:\n          description: \"Invalid input\"\n      security:\n      - petstore_auth:\n        - \"write:pets\"\n        - \"read:pets\"\n    delete:\n      tags:\n      - \"pet\"\n      summary: \"Deletes a pet\"\n      description: \"\"\n      operationId: \"deletePet\"\n      produces:\n      - \"application/xml\"\n      - \"application/json\"\n      parameters:\n      - name: \"api_key\"\n        in: \"header\"\n        required: false\n        type: \"string\"\n      - name: \"petId\"\n        in: \"path\"\n        description: \"Pet id to delete\"\n        required: true\n        type: \"integer\"\n        format: \"int64\"\n      responses:\n        400:\n          description: \"Invalid ID supplied\"\n        404:\n          description: \"Pet not found\"\n      security:\n      - petstore_auth:\n        - \"write:pets\"\n        - \"read:pets\"\n  /pet/{petId}/uploadImage:\n    post:\n      tags:\n      - \"pet\"\n      summary: \"uploads an image\"\n      description: \"\"\n      operationId: \"uploadFile\"\n      consumes:\n      - \"multipart/form-data\"\n      produces:\n      - \"application/json\"\n      parameters:\n      - name: \"petId\"\n        in: \"path\"\n        description: \"ID of pet to update\"\n        required: true\n        type: \"integer\"\n        format: \"int64\"\n      - name: \"additionalMetadata\"\n        in: \"formData\"\n        description: \"Additional data to pass to server\"\n        required: false\n        type: \"string\"\n      - name: \"file\"\n        in: \"formData\"\n        description: \"file to upload\"\n        required: false\n        type: \"file\"\n      responses:\n        200:\n          description: \"successful operation\"\n          schema:\n            $ref: \"#/definitions/ApiResponse\"\n      security:\n      - petstore_auth:\n        - \"write:pets\"\n        - \"read:pets\"\n  /store/inventory:\n    get:\n      tags:\n      - \"store\"\n      summary: \"Returns pet inventories by status\"\n      description: \"Returns a map of status codes to quantities\"\n      operationId: \"getInventory\"\n      produces:\n      - \"application/json\"\n      parameters: []\n      responses:\n        200:\n          description: \"successful operation\"\n          schema:\n            type: \"object\"\n            additionalProperties:\n              type: \"integer\"\n              format: \"int32\"\n      security:\n      - api_key: []\n  /store/order:\n    post:\n      tags:\n      - \"store\"\n      summary: \"Place an order for a pet\"\n      description: \"\"\n      operationId: \"placeOrder\"\n      produces:\n      - \"application/xml\"\n      - \"application/json\"\n      parameters:\n      - in: \"body\"\n        name: \"body\"\n        description: \"order placed for purchasing the pet\"\n        required: true\n        schema:\n          $ref: \"#/definitions/Order\"\n      responses:\n        200:\n          description: \"successful operation\"\n          schema:\n            $ref: \"#/definitions/Order\"\n        400:\n          description: \"Invalid Order\"\n  /store/order/{orderId}:\n    get:\n      tags:\n      - \"store\"\n      summary: \"Find purchase order by ID\"\n      description: \"For valid response try integer IDs with value >= 1 and <= 10.         Other values will generated exceptions\"\n      operationId: \"getOrderById\"\n      produces:\n      - \"application/xml\"\n      - \"application/json\"\n      parameters:\n      - name: \"orderId\"\n        in: \"path\"\n        description: \"ID of pet that needs to be fetched\"\n        required: true\n        type: \"integer\"\n        maximum: 10.0\n        minimum: 1.0\n        format: \"int64\"\n      responses:\n        200:\n          description: \"successful operation\"\n          schema:\n            $ref: \"#/definitions/Order\"\n        400:\n          description: \"Invalid ID supplied\"\n        404:\n          description: \"Order not found\"\n    delete:\n      tags:\n      - \"store\"\n      summary: \"Delete purchase order by ID\"\n      description: \"For valid response try integer IDs with positive integer value.         Negative or non-integer values will generate API errors\"\n      operationId: \"deleteOrder\"\n      produces:\n      - \"application/xml\"\n      - \"application/json\"\n      parameters:\n      - name: \"orderId\"\n        in: \"path\"\n        description: \"ID of the order that needs to be deleted\"\n        required: true\n        type: \"integer\"\n        minimum: 1.0\n        format: \"int64\"\n      responses:\n        400:\n          description: \"Invalid ID supplied\"\n        404:\n          description: \"Order not found\"\n  /user:\n    post:\n      tags:\n      - \"user\"\n      summary: \"Create user\"\n      description: \"This can only be done by the logged in user.\"\n      operationId: \"createUser\"\n      produces:\n      - \"application/xml\"\n      - \"application/json\"\n      parameters:\n      - in: \"body\"\n        name: \"body\"\n        description: \"Created user object\"\n        required: true\n        schema:\n          $ref: \"#/definitions/User\"\n      responses:\n        default:\n          description: \"successful operation\"\n  /user/createWithArray:\n    post:\n      tags:\n      - \"user\"\n      summary: \"Creates list of users with given input array\"\n      description: \"\"\n      operationId: \"createUsersWithArrayInput\"\n      produces:\n      - \"application/xml\"\n      - \"application/json\"\n      parameters:\n      - in: \"body\"\n        name: \"body\"\n        description: \"List of user object\"\n        required: true\n        schema:\n          type: \"array\"\n          items:\n            $ref: \"#/definitions/User\"\n      responses:\n        default:\n          description: \"successful operation\"\n  /user/createWithList:\n    post:\n      tags:\n      - \"user\"\n      summary: \"Creates list of users with given input array\"\n      description: \"\"\n      operationId: \"createUsersWithListInput\"\n      produces:\n      - \"application/xml\"\n      - \"application/json\"\n      parameters:\n      - in: \"body\"\n        name: \"body\"\n        description: \"List of user object\"\n        required: true\n        schema:\n          type: \"array\"\n          items:\n            $ref: \"#/definitions/User\"\n      responses:\n        default:\n          description: \"successful operation\"\n  /user/login:\n    get:\n      tags:\n      - \"user\"\n      summary: \"Logs user into the system\"\n      description: \"\"\n      operationId: \"loginUser\"\n      produces:\n      - \"application/xml\"\n      - \"application/json\"\n      parameters:\n      - name: \"username\"\n        in: \"query\"\n        description: \"The user name for login\"\n        required: true\n        type: \"string\"\n      - name: \"password\"\n        in: \"query\"\n        description: \"The password for login in clear text\"\n        required: true\n        type: \"string\"\n      responses:\n        200:\n          description: \"successful operation\"\n          schema:\n            type: \"string\"\n          headers:\n            X-Rate-Limit:\n              type: \"integer\"\n              format: \"int32\"\n              description: \"calls per hour allowed by the user\"\n            X-Expires-After:\n              type: \"string\"\n              format: \"date-time\"\n              description: \"date in UTC when token expires\"\n        400:\n          description: \"Invalid username/password supplied\"\n  /user/logout:\n    get:\n      tags:\n      - \"user\"\n      summary: \"Logs out current logged in user session\"\n      description: \"\"\n      operationId: \"logoutUser\"\n      produces:\n      - \"application/xml\"\n      - \"application/json\"\n      parameters: []\n      responses:\n        default:\n          description: \"successful operation\"\n  /user/{username}:\n    get:\n      tags:\n      - \"user\"\n      summary: \"Get user by user name\"\n      description: \"\"\n      operationId: \"getUserByName\"\n      produces:\n      - \"application/xml\"\n      - \"application/json\"\n      parameters:\n      - name: \"username\"\n        in: \"path\"\n        description: \"The name that needs to be fetched. Use user1 for testing. \"\n        required: true\n        type: \"string\"\n      responses:\n        200:\n          description: \"successful operation\"\n          schema:\n            $ref: \"#/definitions/User\"\n        400:\n          description: \"Invalid username supplied\"\n        404:\n          description: \"User not found\"\n    put:\n      tags:\n      - \"user\"\n      summary: \"Updated user\"\n      description: \"This can only be done by the logged in user.\"\n      operationId: \"updateUser\"\n      produces:\n      - \"application/xml\"\n      - \"application/json\"\n      parameters:\n      - name: \"username\"\n        in: \"path\"\n        description: \"name that need to be updated\"\n        required: true\n        type: \"string\"\n      - in: \"body\"\n        name: \"body\"\n        description: \"Updated user object\"\n        required: true\n        schema:\n          $ref: \"#/definitions/User\"\n      responses:\n        400:\n          description: \"Invalid user supplied\"\n        404:\n          description: \"User not found\"\n    delete:\n      tags:\n      - \"user\"\n      summary: \"Delete user\"\n      description: \"This can only be done by the logged in user.\"\n      operationId: \"deleteUser\"\n      produces:\n      - \"application/xml\"\n      - \"application/json\"\n      parameters:\n      - name: \"username\"\n        in: \"path\"\n        description: \"The name that needs to be deleted\"\n        required: true\n        type: \"string\"\n      responses:\n        400:\n          description: \"Invalid username supplied\"\n        404:\n          description: \"User not found\"\nsecurityDefinitions:\n  petstore_auth:\n    type: \"oauth2\"\n    authorizationUrl: \"http://petstore.swagger.io/oauth/dialog\"\n    flow: \"implicit\"\n    scopes:\n      write:pets: \"modify pets in your account\"\n      read:pets: \"read your pets\"\n  api_key:\n    type: \"apiKey\"\n    name: \"api_key\"\n    in: \"header\"\ndefinitions:\n  Order:\n    type: \"object\"\n    properties:\n      id:\n        type: \"integer\"\n        format: \"int64\"\n      petId:\n        type: \"integer\"\n        format: \"int64\"\n      quantity:\n        type: \"integer\"\n        format: \"int32\"\n      shipDate:\n        type: \"string\"\n        format: \"date-time\"\n      status:\n        type: \"string\"\n        description: \"Order Status\"\n        enum:\n        - \"placed\"\n        - \"approved\"\n        - \"delivered\"\n      complete:\n        type: \"boolean\"\n        default: false\n    xml:\n      name: \"Order\"\n  Category:\n    type: \"object\"\n    properties:\n      id:\n        type: \"integer\"\n        format: \"int64\"\n      name:\n        type: \"string\"\n    xml:\n      name: \"Category\"\n  User:\n    type: \"object\"\n    properties:\n      id:\n        type: \"integer\"\n        format: \"int64\"\n      username:\n        type: \"string\"\n      firstName:\n        type: \"string\"\n      lastName:\n        type: \"string\"\n      email:\n        type: \"string\"\n      password:\n        type: \"string\"\n      phone:\n        type: \"string\"\n      userStatus:\n        type: \"integer\"\n        format: \"int32\"\n        description: \"User Status\"\n    xml:\n      name: \"User\"\n  Tag:\n    type: \"object\"\n    properties:\n      id:\n        type: \"integer\"\n        format: \"int64\"\n      name:\n        type: \"string\"\n    xml:\n      name: \"Tag\"\n  Pet:\n    type: \"object\"\n    required:\n    - \"name\"\n    - \"photoUrls\"\n    properties:\n      id:\n        type: \"integer\"\n        format: \"int64\"\n      category:\n        $ref: \"#/definitions/Category\"\n      name:\n        type: \"string\"\n        example: \"doggie\"\n      photoUrls:\n        type: \"array\"\n        xml:\n          name: \"photoUrl\"\n          wrapped: true\n        items:\n          type: \"string\"\n      tags:\n        type: \"array\"\n        xml:\n          name: \"tag\"\n          wrapped: true\n        items:\n          $ref: \"#/definitions/Tag\"\n      status:\n        type: \"string\"\n        description: \"pet status in the store\"\n        enum:\n        - \"available\"\n        - \"pending\"\n        - \"sold\"\n    xml:\n      name: \"Pet\"\n  ApiResponse:\n    type: \"object\"\n    properties:\n      code:\n        type: \"integer\"\n        format: \"int32\"\n      type:\n        type: \"string\"\n      message:\n        type: \"string\"\nexternalDocs:\n  description: \"Find out more about Swagger\"\n  url: \"http://swagger.io\""
        r.contentString = "swagger: '2.0'\ninfo:\n  version: 0.0.0\n  title: title\n  description: description\n  termsOfService: terms\n  contact:\n    name: TheAllSeeingPie\n  license:\n    name: MIT\n    url: http://opensource.org/licenses/MIT\nschemes:\n  - http\npaths:\n  /:\n    get:\n      produces: \n        - \"application/json\"\n      responses:\n        200:\n          description: \"Just a simple response\"\n          schema:\n            $ref: \"#/definitions/HelloWorld\"\ndefinitions:\n  HelloWorld:\n    type: \"object\"\n    properties:\n      hello:\n        type: \"string\"\n        enum:\n          - \"world!\""
        r
      }) else Future.value({
        val r = Response()
        r.contentString = compact(render("hello" -> "world!"))
        r
      })
    })
    val dynamicExample = Http.serve(":3000", new GraphQLService(SwaggerSchemaDefinition(":8000")))
    Await.all(testApi, dynamicExample)
  }
}


