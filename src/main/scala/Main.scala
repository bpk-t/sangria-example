import sangria.schema._
import sangria.macros._
import sangria.execution._
import sangria.marshalling.circe._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

object Main extends App {
  case class Picture(
    width: Int,
    height: Int,
    url: Option[String]
  )

  implicit val PictureType = ObjectType(
    "Picture",
    "The Product picture",

    fields[Unit, Picture](
      Field("width", IntType, resolve = _.value.width),
      Field("height", IntType, resolve = _.value.height),
      Field("url", OptionType(StringType), resolve = _.value.url)
    )
  )

  trait Identifiable {
    def id: String
  }

  val IdentifiableType = InterfaceType(
    "Indentifiable",
    "Entity that can be identified",
    fields[Unit, Identifiable](
      Field("id", StringType, resolve = _.value.id)
    )
  )

  case class Product(id: String, name: String, description: String) extends Identifiable {
    def picture(size: Int): Picture =
      Picture(width = size, height = size, url = Some(s"/$size/$id.jpg"))
  }

  import sangria.macros.derive._

  val ProductType =
    deriveObjectType[Unit, Product](
      Interfaces(IdentifiableType),
      IncludeMethods("picture")
    )

  class ProductRepo {
    private val productsSeq = Seq(
      Product("1", "Cheesecake", "Tasty"),
      Product("2", "Health Potion", "+50 HP")
    )

    def product(id: String): Option[Product] = productsSeq.find(_.id == id)
    def products = productsSeq
  }

  val Id = Argument("id", StringType)
  val QueryType = ObjectType("Query", fields[ProductRepo, Unit](
    Field("product", OptionType(ProductType),
      description = Some("Returns a product with specific `id`."),
      arguments = Id :: Nil,
      resolve = c ⇒ c.ctx.product(c arg Id)),

    Field("products", ListType(ProductType),
      description = Some("Returns a list of all available products."),
      resolve = _.ctx.products)))

  val schema = Schema(QueryType)
  val query = graphql"""
      query MyProduct {
        product(id: "2") {
          name
          description

          picture(size: 500) {
            width, height, url
          }
        }

        products {
          name
        }
      }
    """

  val f: Future[io.circe.Json] = Executor.execute(schema, query, new ProductRepo)
  val json = Await.result(f, Duration.Inf)

  println(json.spaces2)
}
