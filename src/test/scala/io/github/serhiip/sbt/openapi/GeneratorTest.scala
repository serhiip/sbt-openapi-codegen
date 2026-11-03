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

  test("specs in different packages may reuse a name") {
    val message = Generator.collisions(
      List(
        ("Widgets", "demo.widgets", Set("Shared")),
        ("Gadgets", "demo.gadgets", Set("Shared"))
      )
    )
    assertEquals(message, Nil)
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
