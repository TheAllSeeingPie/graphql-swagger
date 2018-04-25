package Server

import sangria.macros.derive.deriveObjectType
import sangria.schema.{Argument, BigIntType, Field, ObjectType, OptionType, Schema, StringType, fields}

object Place {
  def find(id: BigInt) = Place(id, "Leeds")

  def place: Field[Unit, Unit] = Field("place", OptionType(deriveObjectType[Unit, Place]()),
    resolve = ctx => Place.find(ctx.args.arg("id")),
    arguments = Argument("id", BigIntType) :: Nil)
}

case class Place(id: BigInt, name: String)

object PlaceSchemaDefinition extends SchemaDefinition {

  import Place.place

  val hello: Field[Unit, Unit] = Field("hello", StringType, resolve = _ => "world!")
  val QueryType = ObjectType("Query", "The root of all queries", fields(hello, place))
  override val schema = Schema(QueryType)
}