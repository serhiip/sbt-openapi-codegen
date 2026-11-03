package io.github.serhiip.sbt.openapi

import io.github.serhiip.sbt.openapi.SchemaModel.{Literal, TypeRef}

import scala.meta.*
import scala.meta.parsers.Parsed

object Trees {
  private val noArgClauses = Nil: List[Term.ArgClause]
  private val noParamClauses = Nil: List[Term.ParamClause]

  def parseType(text: String): Either[String, Type] =
    dialects.Scala3(text).parse[Type] match {
      case Parsed.Success(value)    => Right(value)
      case Parsed.Error(_, message, _) => Left(message)
    }

  def typeOf(tpe: TypeRef, named: String => Type): Type = tpe match {
    case TypeRef.StringType      => t"String"
    case TypeRef.LongType        => t"Long"
    case TypeRef.DoubleType      => t"Double"
    case TypeRef.BooleanType     => t"Boolean"
    case TypeRef.JsonType        => t"io.circe.Json"
    case TypeRef.NullType        => t"JsonNull"
    case TypeRef.Named(name)     => named(name)
    case TypeRef.ListOf(element) => t"List[${typeOf(element, named)}]"
    case TypeRef.MapOf(value)    => t"Map[String, ${typeOf(value, named)}]"
    case TypeRef.Optional(inner) => t"Option[${typeOf(inner, named)}]"
    case TypeRef.Union(members)  => unionOf(members.map(member => typeOf(member, named)))
    case TypeRef.External(scala) =>
      parseType(scala).fold(message => sys.error(s"type override is not a Scala type: $message"), identity)
    case TypeRef.Invalid => t"Nothing"
  }

  def lit(literal: Literal): Term = literal match {
    case Literal.Str(value)     => Lit.String(value)
    case Literal.Whole(value)   => Lit.Long(value)
    case Literal.Decimal(value) => Lit.Double(value)
    case Literal.Bool(value)    => Lit.Boolean(value)
    case Literal.EmptyMap       => q"Map.empty"
  }

  def term(name: String): Term.Name = Term.Name(name)

  def tpe(name: String): Type.Name = Type.Name(name)

  def termRef(dotted: String): Term.Ref = {
    val parts = dotted.split('.').toList
    parts.tail.foldLeft(Term.Name(parts.head): Term.Ref)((acc, part) => Term.Select(acc, Term.Name(part)))
  }

  def source(packageName: String, stats: List[Stat]): Source =
    Source(List(Pkg(termRef(packageName), stats)))

  def print(header: String, tree: Source): String = {
    val body = separateDefinitions(dialects.Scala3(tree).syntax)
    s"$header\n$body\n"
  }

  private def separateDefinitions(source: String): String = {
    val lines = source.split("\n", -1)
    val spaced = lines.foldLeft(List.empty[String]) { (acc, line) =>
      if isTopLevelDefinition(line) && acc.lastOption.exists(_.nonEmpty) then acc :+ "" :+ line
      else acc :+ line
    }
    spaced.mkString("\n")
  }

  private def isTopLevelDefinition(line: String): Boolean =
    line.startsWith("final case class ") ||
      line.startsWith("sealed trait ") ||
      line.startsWith("enum ") ||
      line.startsWith("object ") ||
      line.startsWith("type ") ||
      line.startsWith("class ")

  def wildcardImport(ref: Term.Ref): Import =
    Import(List(Importer(ref, List(Importee.Wildcard()))))

  def givenImport(ref: Term.Ref): Import =
    Import(List(Importer(ref, List(Importee.GivenAll()))))

  def namedImport(ref: Term.Ref, names: List[String]): Import =
    Import(List(Importer(ref, names.map(name => Importee.Name(Name(name))))))

  def givenAlias(name: Option[String], tpe: Type, body: Term): Defn.GivenAlias =
    Defn.GivenAlias(Nil, name.fold(Name.Anonymous())(Term.Name(_)), Nil, tpe, body)

  def obj(name: String, stats: List[Stat]): Defn.Object =
    Defn.Object(Nil, Term.Name(name), Template(None, Nil, Template.Body(None, stats), Nil))

  def parentInits(parents: List[Type]): List[Init] =
    parents.map(tpe => Init(tpe, Name.Anonymous(), noArgClauses))

  def caseClass(name: String, params: List[Term.Param], parents: List[Type]): Defn.Class = {
    val paramss = if params.isEmpty then Nil else List(Term.ParamClause(params))
    Defn.Class(
      List(Mod.Final(), Mod.Case()),
      Type.Name(name),
      Type.ParamClause(Nil),
      Ctor.Primary(Nil, Name.Anonymous(), paramss),
      Template(None, parentInits(parents), Template.Body(None, Nil), Nil)
    )
  }

  def sealedTrait(name: String, parents: List[Type]): Defn.Trait =
    Defn.Trait(
      List(Mod.Sealed()),
      Type.Name(name),
      Type.ParamClause(Nil),
      Ctor.Primary(Nil, Name.Anonymous(), noParamClauses),
      Template(None, parentInits(parents), Template.Body(None, Nil), Nil)
    )

  def enumeration(name: String, valueType: String, cases: List[Stat]): Defn.Enum =
    Defn.Enum(
      Nil,
      Type.Name(name),
      Type.ParamClause(Nil),
      Ctor.Primary(
        Nil,
        Name.Anonymous(),
        List(Term.ParamClause(List(Term.Param(List(Mod.ValParam()), Term.Name("value"), Some(Type.Name(valueType)), None))))
      ),
      Template(None, Nil, Template.Body(None, cases), Nil)
    )

  def enumCase(owner: String, ident: String, params: List[Term.Param], arg: Term): Defn.EnumCase = {
    val paramss = if params.isEmpty then Nil else List(Term.ParamClause(params))
    Defn.EnumCase(
      Nil,
      Term.Name(ident),
      Type.ParamClause(Nil),
      Ctor.Primary(Nil, Name.Anonymous(), paramss),
      List(Init(Type.Name(owner), Name.Anonymous(), List(Term.ArgClause(List(arg)))))
    )
  }

  def exportGiven(name: String): Export =
    Export(List(Importer(Term.Name(name), List(Importee.GivenAll()))))

  def param(name: String, tpe: Type, default: Option[Term]): Term.Param =
    Term.Param(Nil, Term.Name(name), Some(tpe), default)

  private def unionOf(members: List[Type]): Type =
    members.reduceLeft((left, right) => t"$left | $right")
}
