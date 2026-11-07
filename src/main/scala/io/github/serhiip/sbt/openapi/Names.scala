package io.github.serhiip.sbt.openapi

import java.util.regex.Pattern

object Names {
  def scalaName(name: String): String = {
    val parts = name.split("[^A-Za-z0-9]+").filter(_.nonEmpty)
    val pascal = parts.map(part => part.head.toUpper.toString + part.tail).mkString
    if pascal.isEmpty || !pascal.head.isLetter then
      sys.error(s"OpenAPI spec name '$name' cannot be used as a Scala identifier")
    else pascal
  }

  def kebab(name: String): String = {
    val spaced = name.replaceAll("([a-z0-9])([A-Z])", "$1-$2")
    val result = spaced.replaceAll("[^A-Za-z0-9]+", "-").toLowerCase.stripPrefix("-").stripSuffix("-")
    if result.isEmpty then sys.error(s"OpenAPI spec name '$name' is empty")
    else result
  }

  def examplesResource(name: String): String = s"${kebab(name)}-schema-examples.json"

  private[openapi] def fieldScalaName(jsonName: String): String = {
    val camel = camelCase(jsonName)
    if snakeCase(camel) == jsonName then camel else jsonName
  }

  private[openapi] def camelCase(jsonName: String): String = {
    val parts = jsonName.split("_", -1)
    val first = parts.headOption.fold("")(_.toLowerCase)
    val rest = parts.drop(1).map(capitalize)
    first + rest.mkString
  }

  // Same rewrite as io.circe.derivation.renaming.snakeCase, used by Configuration.withSnakeCaseMemberNames.
  private[openapi] def snakeCase(name: String): String = {
    val partial = snakeBase.matcher(name).replaceAll("$1_$2")
    snakeSwap.matcher(partial).replaceAll("$1_$2").toLowerCase
  }

  private def capitalize(part: String): String =
    if part.isEmpty then "" else part.charAt(0).toUpper.toString + part.substring(1)

  private val snakeBase: Pattern = Pattern.compile("([A-Z]+)([A-Z][a-z])")
  private val snakeSwap: Pattern = Pattern.compile("([a-z\\d])([A-Z])")
}
