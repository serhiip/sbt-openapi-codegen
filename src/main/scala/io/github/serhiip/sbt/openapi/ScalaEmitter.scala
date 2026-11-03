package io.github.serhiip.sbt.openapi

import SchemaModel.*
import SchemaModel.TypeRef.*

import scala.collection.mutable
import scala.meta.*

object ScalaEmitter {

  final case class EmitRequest(
      model: Model,
      packageName: String,
      sourceLabel: String,
      scalaName: String,
      unionObject: String,
      examplesResource: String,
      pluginVersion: String,
      options: GeneratorOptions
  )

  final case class GeneratedFiles(main: Map[String, String], testSource: Option[String], examplesJson: Option[String])

  def emit(request: EmitRequest): GeneratedFiles = {
    val model = request.model
    val records = model.definitions.collect { case record: RecordDef => record.name -> record }.toMap
    val enums = model.definitions.collect { case enumDef: EnumDef => enumDef.name -> enumDef }.toMap
    val sums = model.definitions.collect { case sum: SumDef => sum.name -> sum }.toMap
    val places = placement(model.definitions)
    val files = model.definitions.groupBy(defn => places(defn.name).file).map { (path, definitions) =>
      val segment = places(definitions.head.name).segment
      path -> Context(request, records, enums, sums, places, Some(segment)).file(topo(definitions))
    }.toMap
    val unions = collectUnions(model.definitions)
    val root = Context(request, records, enums, sums, places, None)
    val examples =
      if request.options.generateExampleTests then
        Some(root.examplesSuite(model.examples) -> examplesJson(model.examples))
      else None
    val header = s"// AUTO-GENERATED FROM ${request.sourceLabel} by sbt-openapi-codegen ${request.pluginVersion}. Do not edit."
    GeneratedFiles(
      main = files + (
        s"${request.scalaName}.scala" -> aliases(model.definitions, places, request.packageName, header)
      ) + (s"${request.scalaName}UnionCodecs.scala" -> root.unionCodecs(unions)),
      testSource = examples.map(_._1),
      examplesJson = examples.map(_._2)
    )
  }

  private def aliases(definitions: List[Definition], places: Map[String, Place], packageName: String, header: String): String = {
    val exports = definitions.groupBy(defn => places(defn.name).segment).toList.sortBy(_._1).map { (segment, group) =>
      val importees = group.sortBy(_.name).map { defn =>
        val local = places(defn.name).local
        if local == defn.name then Importee.Name(Name(local))
        else Importee.Rename(Name(local), Name(defn.name))
      }
      Export(List(Importer(Trees.termRef(segment), importees)))
    }
    Trees.print(header, Trees.source(packageName, exports))
  }

  private def examplesJson(examples: List[ExampleSpec]): String = {
    val fields = examples.map(example => example.name -> example.json)
    _root_.io.circe.JsonObject.fromIterable(fields).toJson.noSpaces
  }

  private def collectUnions(definitions: List[Definition]): List[Union] = {
    val found = mutable.LinkedHashMap.empty[String, Union]
    def walk(tpe: TypeRef): Unit = tpe match {
      case union @ Union(members) =>
        found.getOrElseUpdate(render(union), union)
        members.foreach(walk)
      case ListOf(element) => walk(element)
      case MapOf(value)    => walk(value)
      case Optional(inner) => walk(inner)
      case _               => ()
    }
    definitions.foreach {
      case record: RecordDef => record.fields.foreach(field => walk(field.tpe))
      case alias: AliasDef   => walk(alias.tpe)
      case _                 => ()
    }
    found.values.toList
  }

  private final case class Place(segment: String, local: String, file: String)

  private val packageKeywords = Set(
    "abstract", "case", "catch", "class", "def", "do", "else", "enum", "export", "extends", "false", "final",
    "finally", "for", "given", "if", "implicit", "import", "lazy", "match", "new", "null", "object", "override",
    "package", "private", "protected", "return", "sealed", "super", "then", "this", "throw", "trait", "true",
    "try", "type", "val", "var", "while", "with", "yield", "derives", "end", "extension", "infix", "inline",
    "opaque", "open", "transparent", "using"
  )

  private val blockedLocals = Set("Codec", "Decoder", "DecodingFailure", "Encoder", "HCursor", "Json", "JsonObject")

  private def placement(definitions: List[Definition]): Map[String, Place] = {
    val byName = definitions.map(defn => defn.name -> defn).toMap
    val segments = ownerSegments(definitions.map(_.owner).distinct)
    val roots = sealedRoots(definitions)
    val segmentOf = definitions.map { defn =>
      val owner = roots.get(defn.name).fold(defn.owner)(root => byName(root).owner)
      defn.name -> segments(owner)
    }.toMap
    val locals = definitions.groupBy(defn => segmentOf(defn.name)).flatMap { case (_, group) =>
      val used = mutable.LinkedHashSet.empty[String]
      group.sortBy(defn => (defn.name != defn.owner, defn.name)).map { defn =>
        val shortened = shorten(defn.owner, defn.name)
        val preferred = if blockedLocals.contains(shortened) then defn.name else shortened
        val local = fresh(preferred, defn.name, used)
        used += local
        defn.name -> local
      }
    }.toMap
    definitions.map { defn =>
      val segment = segmentOf(defn.name)
      val fileName = locals(roots.getOrElse(defn.name, defn.name))
      defn.name -> Place(segment, locals(defn.name), s"${segment.replace('.', '/')}/$fileName.scala")
    }.toMap
  }

  private def ownerSegments(owners: List[String]): Map[String, String] = {
    val used = mutable.LinkedHashSet.empty[String]
    owners.sorted.foldLeft(Map.empty[String, String]) { (acc, owner) =>
      val base = packagePath(owner)
      val segment = uniquePath(base, used)
      used += segment
      acc + (owner -> segment)
    }
  }

  private def packagePath(owner: String): String =
    splitWords(owner).map(packageSegment).mkString(".")

  private def splitWords(name: String): List[String] = {
    val words = List.newBuilder[String]
    val current = new StringBuilder
    name.zipWithIndex.foreach { (char, index) =>
      val next = name.lift(index + 1)
      val splitHere =
        current.nonEmpty && char.isUpper && (current.last.isLower || (current.last.isUpper && next.exists(_.isLower)))
      if splitHere then {
        words += current.result()
        current.clear()
      }
      current += char
    }
    if current.nonEmpty then words += current.result()
    words.result()
  }

  private def uniquePath(base: String, used: mutable.Set[String]): String =
    if !used.contains(base) then base
    else {
      val dot = base.lastIndexOf('.')
      val prefix = if dot < 0 then "" else base.take(dot + 1)
      val leaf = if dot < 0 then base else base.drop(dot + 1)
      Iterator.from(2).map(n => prefix + leaf + n).find(candidate => !used.contains(candidate)).get
    }

  private def packageSegment(word: String): String = {
    val lower = word.toLowerCase
    val ident = if lower.head.isDigit then s"n$lower" else lower
    if packageKeywords.contains(ident) then ident + "_" else ident
  }

  private def shorten(owner: String, name: String): String = {
    val rest = name.stripPrefix(owner)
    if name != owner && rest.nonEmpty && rest.head.isUpper then rest else name
  }

  private def fresh(preferred: String, global: String, used: mutable.LinkedHashSet[String]): String = {
    val candidates = Iterator(preferred, global) ++ Iterator.from(2).map(global + _)
    candidates.find(name => !used.contains(name) && !blockedLocals.contains(name)).get
  }

  private def sealedRoots(definitions: List[Definition]): Map[String, String] = {
    val names = definitions.map(_.name)
    val index = names.zipWithIndex.toMap
    val parent = Array.tabulate(names.size)(identity)
    def find(item: Int): Int =
      if parent(item) == item then item
      else {
        val root = find(parent(item))
        parent(item) = root
        root
      }
    def unite(left: String, right: String): Unit = {
      val leftRoot = find(index(left))
      val rightRoot = find(index(right))
      if leftRoot != rightRoot then parent(leftRoot) = rightRoot
    }
    definitions.foreach {
      case sum: SumDef =>
        sum.variants.foreach(variant => if index.contains(variant) then unite(sum.name, variant))
        sum.parents.foreach(parentName => if index.contains(parentName) then unite(sum.name, parentName))
      case record: RecordDef =>
        record.parents.foreach(parentName => if index.contains(parentName) then unite(record.name, parentName))
      case _ => ()
    }
    definitions.groupBy(defn => find(index(defn.name))).values.flatMap { group =>
      val sums = group.collect { case sum: SumDef => sum }
      if sums.isEmpty then Nil
      else {
        val sumNames = sums.map(_.name).toSet
        val roots = sums.filter(sum => sum.parents.forall(parentName => !sumNames.contains(parentName))).map(_.name).sorted
        val root = roots.headOption.getOrElse(sumNames.min)
        group.map(defn => defn.name -> root)
      }
    }.toMap
  }

  private def topo(definitions: List[Definition]): List[Definition] = {
    val byName = definitions.map(defn => defn.name -> defn).toMap
    val incoming = mutable.Map.empty[String, Int].withDefaultValue(0)
    val edges = mutable.Map.empty[String, List[String]].withDefaultValue(Nil)
    def parentsOf(defn: Definition): List[String] = defn match {
      case record: RecordDef => record.parents
      case sum: SumDef       => sum.parents
      case _                 => Nil
    }
    definitions.foreach { defn =>
      parentsOf(defn).foreach { parentName =>
        if byName.contains(parentName) then {
          edges(parentName) = defn.name :: edges(parentName)
          incoming(defn.name) = incoming(defn.name) + 1
        }
      }
    }
    val ready = mutable.Queue.from(definitions.map(_.name).sorted.filter(name => incoming(name) == 0))
    val ordered = mutable.ListBuffer.empty[Definition]
    while ready.nonEmpty do {
      val name = ready.dequeue()
      ordered += byName(name)
      edges(name).sorted.foreach { child =>
        incoming(child) = incoming(child) - 1
        if incoming(child) == 0 then ready.enqueue(child)
      }
    }
    if ordered.size == definitions.size then ordered.toList else definitions.sortBy(_.name)
  }

  private final class Context(
      request: EmitRequest,
      records: Map[String, RecordDef],
      enums: Map[String, EnumDef],
      sums: Map[String, SumDef],
      places: Map[String, Place],
      segment: Option[String]
  ) {
    private val pkg = segment.fold(request.packageName)(name => s"${request.packageName}.$name")
    private val unionObject = request.unionObject
    private val header = s"// AUTO-GENERATED FROM ${request.sourceLabel} by sbt-openapi-codegen ${request.pluginVersion}. Do not edit."
    private val unionRef = Trees.termRef(s"${request.packageName}.$unionObject")

    private def renderType(tpe: TypeRef): Type = Trees.typeOf(tpe, namedType)

    private def namedType(name: String): Type =
      places.get(name).fold(Type.Name(name))(selectType)

    private def namedTerm(name: String): Term.Ref =
      places.get(name).fold(Term.Name(name))(selectTerm)

    private def selectType(place: Place): Type =
      if segment.contains(place.segment) then Type.Name(place.local)
      else if segment.isEmpty then Type.Select(Trees.termRef(place.segment), Type.Name(place.local))
      else Type.Select(Trees.termRef(s"_root_.${request.packageName}.${place.segment}"), Type.Name(place.local))

    private def selectTerm(place: Place): Term.Ref =
      if segment.contains(place.segment) then Term.Name(place.local)
      else if segment.isEmpty then Term.Select(Trees.termRef(place.segment), Term.Name(place.local))
      else Term.Select(Trees.termRef(s"_root_.${request.packageName}.${place.segment}"), Term.Name(place.local))

    private def localName(name: String): String = places.get(name).fold(name)(_.local)

    def file(definitions: List[Definition]): String = {
      val stats = standardImports ++ definitions.flatMap(emit)
      Trees.print(header, Trees.source(pkg, stats))
    }

    def unionCodecs(unions: List[Union]): String = {
      val givens = unions.zipWithIndex.flatMap(emitUnion)
      val stats = List(
        Trees.wildcardImport(Trees.termRef("io.circe")),
        Trees.wildcardImport(Trees.termRef("io.circe.syntax")),
        Trees.obj(unionObject, erasedHelpers ++ givens),
        Trees.exportGiven(unionObject)
      )
      Trees.print(header, Trees.source(pkg, stats))
    }

    def examplesSuite(examples: List[ExampleSpec]): String = {
      val tests = examples.map { example =>
        val checkType = renderType(example.tpe)
        q"test(${Lit.String(example.name)}) { check[$checkType](${Lit.String(example.name)}) }"
      }
      val resource = Lit.String(request.examplesResource)
      val body = List(
        q"""private val examples: Map[String, Json] = {
          val text = Using.resource(Source.fromResource($resource))(_.mkString)
          parse(text).fold(err => throw err, _.asObject.get.toMap)
        }"""
      ) ++ tests ++ List(checkMethod)
      val suite = Defn.Class(
        Nil,
        Type.Name(s"${request.scalaName}SchemaExamplesSuite"),
        Type.ParamClause(Nil),
        Ctor.Primary(Nil, Name.Anonymous(), Nil: List[Term.ParamClause]),
        Template(
          None,
          List(Init(t"munit.FunSuite", Name.Anonymous(), Nil: List[Term.ArgClause])),
          Template.Body(None, body),
          Nil
        )
      )
      val imports = List(
        Trees.wildcardImport(Trees.termRef("io.circe")),
        Trees.namedImport(Trees.termRef("io.circe.parser"), List("decode", "parse")),
        Trees.wildcardImport(Trees.termRef("io.circe.syntax")),
        Trees.namedImport(Trees.termRef("scala.io"), List("Source")),
        Trees.namedImport(Trees.termRef("scala.util"), List("Using"))
      )
      Trees.print(header, Trees.source(pkg, imports :+ suite))
    }

    private def standardImports: List[Stat] = List(
      Trees.namedImport(
        Trees.termRef("io.circe"),
        List("Codec", "Decoder", "DecodingFailure", "Encoder", "HCursor", "Json", "JsonObject")
      ),
      Trees.wildcardImport(Trees.termRef("io.circe.syntax")),
      Trees.givenImport(unionRef)
    )

    private def emit(definition: Definition): List[Stat] = definition match {
      case record: RecordDef => emitRecord(record)
      case enumDef: EnumDef  => emitEnum(enumDef)
      case sum: SumDef       => emitSum(sum)
      case alias: AliasDef   => List(q"type ${Trees.tpe(localName(alias.name))} = ${renderType(alias.tpe)}")
    }

    private def emitRecord(record: RecordDef): List[Stat] = {
      val params = record.fields.map(fieldParam)
      val explicit = record.fields.size > request.options.explicitCodecFieldThreshold || record.fields.exists(_.syntheticMap)
      val body = if explicit then explicitCodec(record) else List(derivedCodec(record))
      val name = localName(record.name)
      List(
        Trees.caseClass(name, params, record.parents.map(namedType)),
        Trees.obj(name, body)
      )
    }

    private def fieldParam(field: Field): Term.Param = {
      val default =
        if field.syntheticMap then field.defaultLiteral.map(Trees.lit)
        else if !field.required then Some(q"None")
        else field.defaultLiteral.map(Trees.lit)
      Trees.param(field.jsonName, renderType(field.tpe), default)
    }

    private def derivedCodec(record: RecordDef): Stat = {
      val name = namedType(record.name)
      val filter = nullFilter(record)
      Trees.givenAlias(
        None,
        t"Codec.AsObject[$name]",
        q"""{
          val derivedCodec = Codec.AsObject.derived[$name]
          Codec.AsObject.from(derivedCodec, derivedCodec.mapJsonObject(_.filter($filter)))
        }"""
      )
    }

    private def nullFilter(record: RecordDef): Term = {
      val keep = record.fields.filter(field => field.required && field.tpe == JsonType).map(field => Lit.String(field.jsonName))
      if keep.isEmpty then q"!_._2.isNull"
      else q"{ case (key, json) => !json.isNull || Set(..$keep).contains(key) }"
    }

    private def explicitCodec(record: RecordDef): List[Stat] = {
      val plain = record.fields.filterNot(_.syntheticMap)
      val synthetic = record.fields.find(_.syntheticMap)
      val chunks = plain.grouped(request.options.codecChunkSize).toList.zipWithIndex
      val readers = chunks.map { (fields, index) =>
        val enums = fields.zipWithIndex.map { (field, fieldIndex) =>
          val fieldType = renderType(field.tpe)
          val jsonName = Lit.String(field.jsonName)
          val expr =
            if field.required && field.defaultLiteral.isDefined && !isOptional(field.tpe) then
              q"cursor.getOrElse[$fieldType]($jsonName)(${Trees.lit(field.defaultLiteral.get)})"
            else q"cursor.get[$fieldType]($jsonName)"
          enumerator"${Term.Name(s"f$fieldIndex")} <- $expr"
        }
        val terms = fields.indices.map(i => Term.Name(s"f$i")).toList
        val yielded = terms match {
          case one :: Nil => one
          case many       => Term.Tuple(many)
        }
        q"private def ${Term.Name(s"read$index")}(cursor: HCursor) = for (..$enums) yield $yielded"
      }
      val plainArgs = chunks.flatMap { (fields, index) =>
        if fields.size == 1 then List(Term.Name(s"p$index"))
        else fields.indices.map(fieldIndex => q"${Term.Name(s"p$index")}.${Term.Name(s"_${fieldIndex + 1}")}").toList
      }
      val args = plainArgs ++ synthetic.toList.map(_ => Term.Name("extra"))
      val readEnums = chunks.indices.map { index =>
        enumerator"${Term.Name(s"p$index")} <- ${Term.Name(s"read$index")}(cursor)"
      }.toList ++ synthetic.toList.map(_ => enumerator"extra <- readAdditional(cursor)")
      val encoders = chunks.map { (fields, index) =>
        val pairs = fields.map { field =>
          q"${Lit.String(field.jsonName)} -> value.${Trees.term(field.jsonName)}.asJson"
        }
        val body = if pairs.isEmpty then q"Nil" else q"List(..$pairs)"
        q"private def ${Term.Name(s"encoded$index")}(value: ${namedType(record.name)}): List[(String, Json)] = $body"
      }
      val encodedParts = chunks.indices.map(index => q"${Term.Name(s"encoded$index")}(value)").toList
      val knownExpr =
        if encodedParts.isEmpty then q"Nil"
        else encodedParts.reduceLeft[Term]((acc, next) => q"$acc ++ $next")
      val knownKeys = plain.map(field => Lit.String(field.jsonName))
      val additionalReader = synthetic.toList.map { field =>
        val fieldType = renderType(field.tpe)
        q"""private def readAdditional(cursor: HCursor) = {
          val rest = JsonObject.fromIterable(cursor.value.asObject.fold(Nil)(_.toList.filterNot((key, _) => Set(..$knownKeys).contains(key))))
          Decoder[$fieldType].decodeJson(Json.fromJsonObject(rest))
        }"""
      }
      val extraEncode = synthetic match {
        case None =>
          q"JsonObject.fromIterable($knownExpr).filter(${nullFilter(record)})"
        case Some(field) =>
          val fieldType = renderType(field.tpe)
          val fieldName = Trees.term(field.jsonName)
          q"""{
            val known = JsonObject.fromIterable($knownExpr).filter(${nullFilter(record)})
            val extra = Encoder[$fieldType].apply(value.$fieldName).asObject.getOrElse(JsonObject.empty)
            JsonObject.fromIterable(extra.toList.filterNot((key, _) => Set(..$knownKeys).contains(key)) ++ known.toList)
          }"""
      }
      val codec = Trees.givenAlias(
        None,
        t"Codec.AsObject[${namedType(record.name)}]",
        q"""Codec.AsObject.from(
          Decoder.instance { cursor =>
            for (..$readEnums) yield ${namedTerm(record.name)}(..$args)
          },
          Encoder.AsObject.instance { value => $extraEncode }
        )"""
      )
      readers ++ encoders ++ additionalReader ++ List(codec)
    }

    private def emitEnum(enumDef: EnumDef): List[Stat] = {
      val name = localName(enumDef.name)
      val cases = enumDef.cases.map { enumCase =>
        Trees.enumCase(name, enumCase.ident, Nil, Trees.lit(enumCase.literal))
      }
      val unknown = Trees.enumCase(
        name,
        enumDef.unknownIdent,
        List(Trees.param("raw", Type.Name(enumDef.valueType), None)),
        Term.Name("raw")
      )
      val (encode, decode) = enumDef.valueType match {
        case "String" => (q"Encoder.encodeString", q"Decoder.decodeString")
        case "Long"   => (q"Encoder.encodeLong", q"Decoder.decodeLong")
        case _        => (q"Encoder.encodeDouble", q"Decoder.decodeDouble")
      }
      val rendered = namedType(enumDef.name)
      val known = enumDef.cases.map(enumCase => Term.Name(enumCase.ident))
      val companion = List(
        q"val known: List[$rendered] = List(..$known)",
        Trees.givenAlias(None, t"Encoder[$rendered]", q"$encode.contramap(_.value)"),
        Trees.givenAlias(None, t"Decoder[$rendered]", q"$decode.map { text => known.find(_.value == text).getOrElse(${Trees.term(enumDef.unknownIdent)}(text)) }")
      )
      List(Trees.enumeration(name, enumDef.valueType, cases :+ unknown), Trees.obj(name, companion))
    }

    private def emitSum(sum: SumDef): List[Stat] = {
      val variants = sum.variants.filter(name => records.contains(name) || sums.contains(name))
      val recordsFirst = variants.filter(records.contains) ++ variants.filter(sums.contains)
      val encoderCases = recordsFirst.map { name =>
        val variant = namedType(name)
        p"case value: $variant => Encoder.AsObject[$variant].encodeObject(value)"
      }
      val decoder = sum.discriminator match {
        case Some(discriminator) => discriminatorDecoder(sum, discriminator)
        case None                => orDecoder(sum, variants)
      }
      val name = localName(sum.name)
      val rendered = namedType(sum.name)
      val companion = List(
        Trees.givenAlias(None, t"Encoder.AsObject[$rendered]", q"Encoder.AsObject.instance { ..case $encoderCases }"),
        Trees.givenAlias(None, t"Decoder[$rendered]", decoder)
      )
      List(Trees.sealedTrait(name, sum.parents.map(namedType)), Trees.obj(name, companion))
    }

    private def discriminatorDecoder(sum: SumDef, discriminator: Discriminator): Term = {
      val cases = discriminator.tags.map { (tag, name) =>
        p"case ${Lit.String(tag)} => cursor.as[${namedType(name)}]"
      }
      val fallback = p"""case other => Left(DecodingFailure(${Lit.String(s"Unknown ${sum.name} ${discriminator.property}: ")} + other, cursor.history))"""
      val property = Lit.String(discriminator.property)
      q"""Decoder.instance { cursor =>
        cursor.get[String]($property).flatMap { ..case ${cases :+ fallback} }
      }"""
    }

    private def orDecoder(sum: SumDef, variants: List[String]): Term = {
      val ordered = variants.zipWithIndex.sortBy { (name, index) =>
        val requiredCount = records.get(name).fold(0)(_.fields.count(_.required))
        (-requiredCount, index)
      }.map(_._1)
      val parts = ordered.map { name =>
        val variant = namedType(name)
        val sumType = namedType(sum.name)
        q"Decoder[$variant].map[$sumType](value => value)"
      }
      parts.tail.foldLeft(parts.head)((acc, next) => q"$acc.or($next)")
    }

    private def emitUnion(indexed: (Union, Int)): List[Stat] = {
      val (union, index) = indexed
      val rendered = renderType(union)
      val members = union.members
      val allStringEnums = members.forall {
        case Named(name) => enums.get(name).exists(_.valueType == "String")
        case _           => false
      }
      val decoder =
        if allStringEnums then stringEnumDecoder(rendered, members)
        else {
          val parts = members.map(member => memberDecoder(member, rendered))
          parts.tail.foldLeft(parts.head)((acc, next) => q"$acc.or($next)")
        }
      List(
        Trees.givenAlias(Some(s"unionEncoder$index"), t"Encoder[$rendered]", q"Encoder.instance { ..case ${encodeBody(members)} }"),
        Trees.givenAlias(Some(s"unionDecoder$index"), t"Decoder[$rendered]", decoder)
      )
    }

    private def stringEnumDecoder(rendered: Type, members: List[TypeRef]): Term = {
      val lookups = members.collect { case Named(name) =>
        val renderedType = rendered
        q"${namedTerm(name)}.known.find(_.value == text).map(value => (value: $renderedType))"
      }
      val first = members.collectFirst { case Named(name) => name }.get
      val unknown = enums(first).unknownIdent
      val chain = lookups.tail.foldLeft(lookups.head)((acc, next) => q"$acc.orElse($next)")
      q"""Decoder.instance { cursor =>
        cursor.as[String].map { text =>
          $chain.getOrElse((${namedTerm(first)}.${Trees.term(unknown)}(text): $rendered))
        }
      }"""
    }

    private def memberDecoder(tpe: TypeRef, rendered: Type): Term = tpe match {
      case Named(name) if enums.get(name).exists(_.valueType == "String") =>
        q"""Decoder.decodeString.emap(text => ${namedTerm(name)}.known.find(_.value == text).toRight("Unknown " + text)).map[$rendered](value => value)"""
      case Named(name) if enums.get(name).exists(_.valueType == "Long") =>
        q"""Decoder.decodeLong.emap(number => ${namedTerm(name)}.known.find(_.value == number).toRight("Unknown " + number)).map[$rendered](value => value)"""
      case Named(name) if enums.get(name).exists(_.valueType == "Double") =>
        q"""Decoder.decodeDouble.emap(number => ${namedTerm(name)}.known.find(_.value == number).toRight("Unknown " + number)).map[$rendered](value => value)"""
      case other =>
        val member = renderType(other)
        q"Decoder[$member].map[$rendered](value => value)"
    }

    private def encodeBody(members: List[TypeRef]): List[Case] = {
      val lists = members.collect { case list @ ListOf(_) => list }
      val maps = members.collect { case map @ MapOf(_) => map }
      members.foldLeft((List.empty[Case], false, false)) { case ((acc, listsDone, mapsDone), member) =>
        member match {
          case ListOf(_) if !listsDone => (acc :+ listCase(lists), true, mapsDone)
          case ListOf(_)               => (acc, listsDone, mapsDone)
          case MapOf(_) if !mapsDone   => (acc :+ mapCase(maps), listsDone, true)
          case MapOf(_)                => (acc, listsDone, mapsDone)
          case other                   => (acc :+ simpleCase(other), listsDone, mapsDone)
        }
      }._1
    }

    private def simpleCase(tpe: TypeRef): Case = {
      val rendered = renderType(tpe)
      tpe match {
        case StringType  => p"case value: String => Encoder[String].apply(value)"
        case LongType    => p"case value: Long => Encoder[Long].apply(value)"
        case DoubleType  => p"case value: Double => Encoder[Double].apply(value)"
        case BooleanType => p"case value: Boolean => Encoder[Boolean].apply(value)"
        case JsonType    => p"case value: io.circe.Json => Encoder[io.circe.Json].apply(value)"
        case NullType    => p"case value: JsonNull => Encoder[JsonNull].apply(value)"
        case Named(_)    => p"case value: $rendered => Encoder[$rendered].apply(value)"
        case Optional(_) => p"case value: Option[?] => Encoder[$rendered].apply(value.asInstanceOf[$rendered])"
        case External(_) => p"case value: $rendered => Encoder[$rendered].apply(value.asInstanceOf[$rendered])"
        case other       => sys.error(s"No encoder case for ${render(other)}")
      }
    }

    private def listCase(lists: List[ListOf]): Case =
      if lists.size == 1 then {
        val rendered = renderType(lists.head)
        p"case value: List[?] => Encoder[$rendered].apply(value.asInstanceOf[$rendered])"
      } else {
        val empty = q"Encoder[${renderType(lists.head)}].apply(Nil)"
        val arms = lists.map { list =>
          val rendered = renderType(list)
          val pred = elementPredicate(list.element, "head")
          q"((head: Any) => $pred, (xs: List[?]) => Encoder[$rendered].apply(xs.asInstanceOf[$rendered]))"
        }
        val call = q"${Trees.term(unionObject)}.encodeErasedList(value, $empty, ..$arms)"
        p"case value: List[?] => $call"
      }

    private def mapCase(maps: List[MapOf]): Case =
      if maps.size == 1 then {
        val rendered = renderType(maps.head)
        p"case value: Map[?, ?] => Encoder[$rendered].apply(value.asInstanceOf[$rendered])"
      } else {
        val empty = q"Encoder[${renderType(maps.head)}].apply(Map.empty)"
        val arms = maps.map { map =>
          val rendered = renderType(map)
          val pred = elementPredicate(map.value, "head")
          q"((head: Any) => $pred, (xs: Map[String, ?]) => Encoder[$rendered].apply(xs.asInstanceOf[$rendered]))"
        }
        val call = q"${Trees.term(unionObject)}.encodeErasedMap(value.asInstanceOf[Map[String, ?]], $empty, ..$arms)"
        p"case value: Map[?, ?] => $call"
      }

    private def elementPredicate(tpe: TypeRef, expr: String): Term = {
      val subject = Term.Name(expr)
      tpe match {
        case StringType  => q"$subject.isInstanceOf[String]"
        case LongType    => q"$subject.isInstanceOf[Long]"
        case DoubleType  => q"$subject.isInstanceOf[Double]"
        case BooleanType => q"$subject.isInstanceOf[Boolean]"
        case JsonType    => q"$subject.isInstanceOf[io.circe.Json]"
        case NullType    => q"$subject.isInstanceOf[JsonNull]"
        case Named(name) => q"$subject.isInstanceOf[${namedType(name)}]"
        case ListOf(element) =>
          val pred = elementPredicate(element, "inner")
          q"$subject.isInstanceOf[List[?]] && $subject.asInstanceOf[List[?]].headOption.forall(inner => $pred)"
        case MapOf(value) =>
          val pred = elementPredicate(value, "inner")
          q"$subject.isInstanceOf[scala.collection.Map[?, ?]] && $subject.asInstanceOf[scala.collection.Map[?, ?]].values.headOption.forall(inner => $pred)"
        case Optional(inner) =>
          val pred = elementPredicate(inner, "inner")
          q"$subject.isInstanceOf[Option[?]] && $subject.asInstanceOf[Option[?]].forall(inner => $pred)"
        case _ => q"false"
      }
    }

    private def isOptional(tpe: TypeRef): Boolean = tpe match {
      case Optional(_) => true
      case _           => false
    }

    private def erasedHelpers: List[Stat] = List(
      q"""def encodeErasedList(values: List[?], ifEmpty: Json, arms: ((Any => Boolean, List[?] => Json))*): Json =
        values.headOption match {
          case None => ifEmpty
          case Some(head) =>
            arms.collectFirst { case (matches, encode) if matches(head) => encode(values) }.getOrElse(
              throw IllegalArgumentException("No union arm matched list element " + head.getClass.getName)
            )
        }""",
      q"""def encodeErasedMap(values: Map[String, ?], ifEmpty: Json, arms: ((Any => Boolean, Map[String, ?] => Json))*): Json =
        values.values.headOption match {
          case None => ifEmpty
          case Some(head) =>
            arms.collectFirst { case (matches, encode) if matches(head) => encode(values) }.getOrElse(
              throw IllegalArgumentException("No union arm matched map value " + head.getClass.getName)
            )
        }"""
    )

    private def checkMethod: Stat =
      q"""private def check[A: Decoder: Encoder](name: String): Unit = {
        val json = examples(name)
        val decoded = decode[A](json.noSpaces).fold(err => fail(name + " did not decode: " + err.getMessage), identity)
        val again = decode[A](Encoder[A].apply(decoded).noSpaces).fold(err => fail(name + " did not round-trip: " + err.getMessage), identity)
        assertEquals(again, decoded)
      }"""
  }
}
