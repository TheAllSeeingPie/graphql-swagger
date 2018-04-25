package Server

import sangria.macros.derive.deriveObjectType
import sangria.schema.{Argument, BigIntType, Field, ObjectType, OptionType, Schema, StringType, fields}

object Person {
  def find(id: BigInt) = Person(id, "Bob")

  def person: Field[Unit, Unit] = Field("person", OptionType(deriveObjectType[Unit, Person]()),
    resolve = ctx => Person.find(ctx.args.arg("id")),
    arguments = Argument("id", BigIntType) :: Nil)
}

case class Person(id: BigInt, name: String)

object PersonSchemaDefinition extends SchemaDefinition {

  import Person.person

  val hello: Field[Unit, Unit] = Field("hello", StringType, resolve = _ => "world!")
  val QueryType = ObjectType("Query", "The root of all queries", fields(hello, person))
  override val schema = Schema(QueryType)
}
