package io.github.serhiip.sbt.openapi

import scala.io.Source as InputSource
import scala.meta.*
import scala.meta.parsers.Parsed
import scala.util.Using

class GeneratorTest extends munit.FunSuite {

  private def text(name: String): String =
    Using.resource(InputSource.fromResource(s"specs/$name"))(_.mkString)

  private def spec(name: String, packageName: String = "demo"): OpenApiSpec =
    OpenApiSpec(name, OpenApiSource.FromFile(new java.io.File(name)), packageName)

  private def generated(resource: String, name: String): ScalaEmitter.GeneratedFiles =
    Generator.generate(text(resource), resource, spec(name), resource, "test").fold(err => fail(err.mkString("\n")), _.files)

  test("OpenAPI 3.0 YAML generates allOf, oneOf, nullable and enum") {
    val files = generated("shop.yaml", "Shop")
    val source = files.main.values.mkString("\n")
    assert(source.contains("sealed trait Animal"))
    assert(source.contains("enum PetStatus"))
    assert(source.contains("nickname: Option[String]"))
    assert(files.testSource.exists(_.contains("class ShopSchemaExamplesSuite")))
    assert(files.examplesJson.exists(_.contains("doggo")))
  }

  test("OpenAPI 3.1 JSON generates type arrays and const") {
    val source = generated("counts.json", "Counts").main.values.mkString("\n")
    assert(source.contains("value: Option[Long]"))
    assert(source.contains(""""count""""))
  }

  test("Petstore 3.0 generates the Pet schema and its example") {
    val files = generated("petstore.json", "Petstore")
    val source = files.main.values.mkString("\n")
    assert(source.contains("final case class Pet"))
    assert(source.contains("final case class Order"))
    assert(files.testSource.exists(_.contains("check[pet.Pet]")))
    assert(files.main.contains("pet/Pet.scala"))
  }

  test("Swagger 2.0 is rejected") {
    val result = Generator.generate(text("swagger.yaml"), "swagger.yaml", spec("Old"), "swagger.yaml", "test")
    assertEquals(result.left.map(_.mkString), Left("Swagger 2.0 specs are not supported; convert the spec to OpenAPI 3"))
  }

  test("a $ref outside components.schemas reports its path") {
    val result = Generator.generate(text("bad-ref.json"), "bad-ref.json", spec("Bad"), "bad-ref.json", "test")
    val message = result.left.map(_.mkString("\n")).fold(identity, _ => "")
    assert(message.contains("Thing"))
    assert(message.contains("#/components/parameters/Nope"))
  }

  test("two specs that emit the same name in one package collide") {
    val message = Generator.collisions(
      List(
        ("Widgets", "demo.api", Set("Widget", "Shared")),
        ("Gadgets", "demo.api", Set("Gadget", "Shared"))
      )
    )
    assertEquals(message, List("Widgets and Gadgets both generate Shared in package demo.api"))
  }

  test("emitted sources parse as Scala 3") {
    List("shop.yaml" -> "Shop", "counts.json" -> "Counts", "petstore.json" -> "Petstore").foreach { (resource, name) =>
      assertParses(generated(resource, name))
    }
  }

  test("keywords, odd property names and enum text are quoted") {
    val spec = OpenApiSpec(
      "Odd",
      OpenApiSource.FromFile(new java.io.File("odd.json")),
      "demo",
      options = GeneratorOptions(generateExampleTests = false)
    )
    val files = Generator.generate(oddSpec, "odd.json", spec, "odd.json", "test").fold(err => fail(err.mkString("\n")), _.files)
    assertParses(files)
    val trees = files.main.values.map(source => dialects.Scala3(source).parse[Source].get)
    val names = trees.flatMap(_.collect { case name: Term.Name => name.value }).toSet
    assert(names.contains("type"))
    assert(names.contains("a-b"))
    assert(names.contains("x$y"))
    assert(names.contains("has space"))
    assert(names.contains("quote\"name"))
    val texts = trees.flatMap(_.collect { case Lit.String(value) => value }).toSet
    assert(texts.contains("say \"hi\""))
    assert(texts.contains("a\\b"))
    assert(texts.contains("line\n"))
    assert(texts.contains("u\u0001"))
  }

  test("inline schemas nest under the component that created them") {
    val spec = OpenApiSpec(
      "Nested",
      OpenApiSource.FromFile(new java.io.File("nested.json")),
      "demo",
      options = GeneratorOptions(generateExampleTests = false)
    )
    val files = Generator.generate(nestedSpec, "nested.json", spec, "nested.json", "test").fold(err => fail(err.mkString("\n")), _.files)
    assertParses(files)
    val chat = files.main("chat/request/ChatRequest.scala")
    val reasoning = files.main("chat/request/Reasoning.scala")
    assert(chat.contains("reasoning: Option[Reasoning]"))
    assert(chat.contains("_root_.demo.provider.Provider"))
    assert(reasoning.contains("final case class Reasoning"))
    assert(reasoning.contains("\n\nobject Reasoning"))
    assert(reasoning.contains("effort: Option[ReasoningEffort]"))
    assert(files.main("chat/request/ReasoningEffort.scala").contains("enum ReasoningEffort"))
    assert(files.main.contains("provider/Provider.scala"))
    val animal = files.main("animal/Animal.scala")
    assert(animal.contains("sealed trait Animal"))
    assert(animal.contains("}\n\nfinal case class Cat"))
    assert(animal.contains("final case class Cat"))
    assert(animal.contains("final case class Dog"))
    assert(!files.main.keys.exists(_.startsWith("cat/")))
    val aliases = files.main("Nested.scala")
    assert(aliases.contains("export chat.request."))
    assert(aliases.contains("Reasoning as ChatRequestReasoning"))
    assert(aliases.contains("export provider.Provider"))
    assert(aliases.contains("export animal.{Animal, Cat, Dog}"))
  }

  test("an invalid type override is reported") {
    val spec = OpenApiSpec(
      "Counts",
      OpenApiSource.FromFile(new java.io.File("counts.json")),
      "demo",
      typeOverrides = Map("Count" -> "List[")
    )
    val result = Generator.generate(text("counts.json"), "counts.json", spec, "counts.json", "test")
    val message = result.left.map(_.mkString("\n")).fold(identity, _ => "")
    assert(message.contains("Count"))
    assert(message.contains("type override is not a Scala type"))
  }

  test("camelCase field names round-trip through circe snake case") {
    assertEquals(Names.fieldScalaName("total_cost"), "totalCost")
    assertEquals(Names.fieldScalaName("is_byok"), "isByok")
    assertEquals(Names.fieldScalaName("foo2_bar"), "foo2Bar")
    assertEquals(Names.fieldScalaName("v2_api"), "v2Api")
    assertEquals(Names.fieldScalaName("1_foo"), "1Foo")
    assertEquals(Names.snakeCase("totalCost"), "total_cost")
    assertEquals(Names.snakeCase("isByok"), "is_byok")
    assertEquals(Names.snakeCase("foo2Bar"), "foo2_bar")
    assertEquals(Names.snakeCase("v2Api"), "v2_api")
    assertEquals(Names.snakeCase("1Foo"), "1_foo")
    assertEquals(Names.snakeCase("HTTPResponse"), "http_response")
    assertEquals(Names.snakeCase("alreadyCamel"), "already_camel")
    assertEquals(Names.snakeCase("a__b"), "a__b")
    assertEquals(Names.snakeCase("uptimeLast5m"), "uptime_last5m")
    assertEquals(Names.fieldScalaName("alreadyCamel"), "alreadyCamel")
    assertEquals(Names.fieldScalaName("HTTPResponse"), "HTTPResponse")
    assertEquals(Names.fieldScalaName("a__b"), "a__b")
    assertEquals(Names.fieldScalaName("uptime_last_5m"), "uptime_last_5m")
    assertEquals(Names.fieldScalaName("ephemeral_1h_input_tokens"), "ephemeral_1h_input_tokens")
    assertEquals(Names.fieldScalaName("latency_last_30m"), "latency_last_30m")
    assertEquals(Names.fieldScalaName("input_cache_write_1h"), "input_cache_write_1h")
    assertEquals(Names.fieldScalaName("additionalProperties"), "additionalProperties")
    assertEquals(Names.snakeCase(Names.fieldScalaName("a__b")), "a__b")
    assertEquals(Names.snakeCase(Names.fieldScalaName("latency_last_30m")), "latency_last_30m")
  }

  test("field names stay as in the spec unless camelCase is selected") {
    val text = dataSpec("total_cost" -> "string", "is_byok" -> "boolean", "totalCost" -> "string")
    val source = emit(text, GeneratorOptions(fieldNaming = FieldNaming.AsInSpec, generateExampleTests = false)).main.values.mkString("\n")
    assert(source.contains("total_cost:"))
    assert(source.contains("is_byok:"))
    assert(source.contains("totalCost:"))
    assert(source.contains("Codec.AsObject.derived"))
    assert(!source.contains("ConfiguredCodec"))
    assert(!source.contains("withSnakeCaseMemberNames"))
  }

  test("camelCase renames snake_case fields and derives a configured codec") {
    val text =
      """{
        |  "openapi": "3.1.0",
        |  "info": { "title": "Names", "version": "1" },
        |  "components": {
        |    "schemas": {
        |      "Data": {
        |        "type": "object",
        |        "required": ["total_cost", "raw_payload"],
        |        "properties": {
        |          "total_cost": { "type": "string" },
        |          "is_byok": { "type": "boolean" },
        |          "foo2_bar": { "type": "string" },
        |          "v2_api": { "type": "string" },
        |          "a__b": { "type": "string" },
        |          "uptime_last_5m": { "type": "string" },
        |          "raw_payload": {}
        |        }
        |      }
        |    }
        |  }
        |}""".stripMargin
    val files = emit(text, GeneratorOptions(fieldNaming = FieldNaming.CamelCase, generateExampleTests = false))
    assertParses(files)
    val source = files.main.values.mkString("\n")
    assert(source.contains("totalCost: String"))
    assert(source.contains("isByok: Option[Boolean]"))
    assert(source.contains("foo2Bar:"))
    assert(source.contains("v2Api:"))
    assert(source.contains("a__b:"))
    assert(source.contains("uptime_last_5m:"))
    assert(source.contains("rawPayload: io.circe.Json"))
    assert(source.contains("import io.circe.derivation.{Configuration, ConfiguredCodec}"))
    assert(source.contains("given Configuration = Configuration.default.withSnakeCaseMemberNames"))
    assert(source.contains("ConfiguredCodec.derived"))
    assert(source.contains("mapJsonObject"))
    assert(source.contains("\"raw_payload\""))
    assert(!source.contains("Codec.AsObject.derived"))
    assert(!source.contains("total_cost:"))
    assert(!source.contains("is_byok:"))
    assert(!source.contains("\"rawPayload\""))
  }

  test("explicit codecs keep JSON names and read camelCase accessors") {
    val files = emit(
      dataSpec("total_cost" -> "string", "is_byok" -> "boolean"),
      GeneratorOptions(fieldNaming = FieldNaming.CamelCase, explicitCodecFieldThreshold = 1, generateExampleTests = false)
    )
    assertParses(files)
    val source = files.main.values.mkString("\n")
    assert(source.contains("totalCost:"))
    assert(source.contains("isByok:"))
    assert(source.contains("value.totalCost"))
    assert(source.contains("value.isByok"))
    assert(source.contains("\"total_cost\""))
    assert(source.contains("\"is_byok\""))
    assert(!source.contains("value.total_cost"))
    assert(!source.contains("value.is_byok"))
    assert(!source.contains("ConfiguredCodec"))
    assert(!source.contains("withSnakeCaseMemberNames"))
  }

  test("names that are not plain snake case keep their spec spelling") {
    val files = emit(
      dataSpec("total_cost" -> "string", "alreadyCamel" -> "string", "HTTPResponse" -> "string"),
      GeneratorOptions(fieldNaming = FieldNaming.CamelCase, generateExampleTests = false)
    )
    assertParses(files)
    val source = files.main.values.mkString("\n")
    assert(source.contains("totalCost:"))
    assert(source.contains("alreadyCamel:"))
    assert(source.contains("HTTPResponse:"))
    assert(source.contains("value.totalCost"))
    assert(source.contains("value.alreadyCamel"))
    assert(source.contains("value.HTTPResponse"))
    assert(source.contains("\"total_cost\""))
    assert(source.contains("\"alreadyCamel\""))
    assert(source.contains("\"HTTPResponse\""))
    assert(!source.contains("withSnakeCaseMemberNames"))
    assert(!source.contains("totalcost"))
    assert(!source.contains("httpresponse"))
  }

  test("camelCase quotes keywords and leading digits") {
    val files = emit(
      dataSpec("type" -> "string", "class" -> "string", "1_foo" -> "string"),
      GeneratorOptions(fieldNaming = FieldNaming.CamelCase, generateExampleTests = false)
    )
    assertParses(files)
    val source = files.main.values.mkString("\n")
    assert(source.contains("`type`"))
    assert(source.contains("`class`"))
    assert(source.contains("`1Foo`"))
    assert(source.contains("withSnakeCaseMemberNames"))
  }

  test("additionalProperties uses the same Scala name as other fields") {
    val text =
      """{
        |  "openapi": "3.1.0",
        |  "info": { "title": "Names", "version": "1" },
        |  "components": {
        |    "schemas": {
        |      "Data": {
        |        "type": "object",
        |        "properties": { "total_cost": { "type": "string" } },
        |        "additionalProperties": { "type": "string" }
        |      }
        |    }
        |  }
        |}""".stripMargin
    val files = emit(text, GeneratorOptions(fieldNaming = FieldNaming.CamelCase, generateExampleTests = false))
    assertParses(files)
    val source = files.main.values.mkString("\n")
    assert(source.contains("totalCost:"))
    assert(source.contains("additionalProperties: Map[String, String] = Map.empty"))
    assert(source.contains("value.totalCost"))
    assert(source.contains("value.additionalProperties"))
    assert(source.contains("\"total_cost\""))
    assert(!source.contains("additionalproperties"))
    assert(!source.contains("withSnakeCaseMemberNames"))
  }

  test("camelCase field names that collide fail with both JSON names") {
    val text = dataSpec("total_cost" -> "string", "totalCost" -> "string")
    val openApi = OpenApiSpec(
      "Names",
      OpenApiSource.FromFile(new java.io.File("names.json")),
      "demo",
      options = GeneratorOptions(fieldNaming = FieldNaming.CamelCase, generateExampleTests = false)
    )
    Generator.generate(text, "names.json", openApi, "names.json", "test") match {
      case Left(errors) =>
        val message = errors.mkString("\n")
        assert(message.contains("total_cost and totalCost"))
        assert(message.contains("totalCost"))
      case Right(_) => fail("expected field name collision")
    }
  }

  test("a camelCase property that clashes with additionalProperties fails with both JSON names") {
    val text =
      """{
        |  "openapi": "3.1.0",
        |  "info": { "title": "Names", "version": "1" },
        |  "components": {
        |    "schemas": {
        |      "Data": {
        |        "type": "object",
        |        "properties": { "additional_properties": { "type": "string" } },
        |        "additionalProperties": { "type": "string" }
        |      }
        |    }
        |  }
        |}""".stripMargin
    val openApi = OpenApiSpec(
      "Names",
      OpenApiSource.FromFile(new java.io.File("names.json")),
      "demo",
      options = GeneratorOptions(fieldNaming = FieldNaming.CamelCase, generateExampleTests = false)
    )
    Generator.generate(text, "names.json", openApi, "names.json", "test") match {
      case Left(errors) =>
        val message = errors.mkString("\n")
        assert(message.contains("additional_properties and additionalProperties"))
      case Right(_) => fail("expected field name collision")
    }
  }

  test("specs in different packages may reuse a name") {
    val message = Generator.collisions(
      List(
        ("Widgets", "demo.widgets", Set("Shared")),
        ("Gadgets", "demo.gadgets", Set("Shared"))
      )
    )
    assertEquals(message, Nil)
  }

  private def emit(text: String, options: GeneratorOptions): ScalaEmitter.GeneratedFiles = {
    val openApi = OpenApiSpec(
      "Names",
      OpenApiSource.FromFile(new java.io.File("names.json")),
      "demo",
      options = options
    )
    Generator.generate(text, "names.json", openApi, "names.json", "test").fold(err => fail(err.mkString("\n")), _.files)
  }

  private def dataSpec(properties: (String, String)*): String = {
    val props = properties.map { (name, tpe) =>
      s""""${name.replace("\\", "\\\\").replace("\"", "\\\"")}": { "type": "$tpe" }"""
    }.mkString(", ")
    s"""{
       |  "openapi": "3.1.0",
       |  "info": { "title": "Names", "version": "1" },
       |  "components": { "schemas": { "Data": { "type": "object", "properties": { $props } } } }
       |}""".stripMargin
  }

  private def assertParses(files: ScalaEmitter.GeneratedFiles): Unit = {
    val sources = files.main.values.toList ++ files.testSource.toList
    sources.foreach { source =>
      dialects.Scala3(source).parse[Source] match {
        case Parsed.Success(_) => ()
        case Parsed.Error(pos, message, _) => fail(s"$message at $pos\n$source")
      }
    }
  }

  private val oddSpec: String =
    """{
      |  "openapi": "3.1.0",
      |  "info": { "title": "Odd", "version": "1" },
      |  "components": {
      |    "schemas": {
      |      "Odd": {
      |        "type": "object",
      |        "properties": {
      |          "type": { "type": "string" },
      |          "a-b": { "type": "string" },
      |          "x$y": { "type": "string" },
      |          "has space": { "type": "string" },
      |          "quote\"name": { "type": "string" },
      |          "status": { "type": "string", "enum": ["say \"hi\"", "a\\b", "line\n", "u\u0001"] }
      |        }
      |      }
      |    }
      |  }
      |}""".stripMargin

  private val nestedSpec: String =
    """{
      |  "openapi": "3.1.0",
      |  "info": { "title": "Nested", "version": "1" },
      |  "components": {
      |    "schemas": {
      |      "ChatRequest": {
      |        "type": "object",
      |        "properties": {
      |          "reasoning": {
      |            "type": "object",
      |            "properties": {
      |              "effort": { "type": "string", "enum": ["low", "high"] }
      |            }
      |          },
      |          "provider": { "$ref": "#/components/schemas/Provider" }
      |        }
      |      },
      |      "Provider": {
      |        "type": "object",
      |        "properties": { "name": { "type": "string" } }
      |      },
      |      "Animal": {
      |        "oneOf": [
      |          { "$ref": "#/components/schemas/Cat" },
      |          { "$ref": "#/components/schemas/Dog" }
      |        ]
      |      },
      |      "Cat": { "type": "object", "properties": { "name": { "type": "string" } } },
      |      "Dog": { "type": "object", "properties": { "name": { "type": "string" } } }
      |    }
      |  }
      |}""".stripMargin
}
