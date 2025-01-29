package top.harryxi.toml

import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.OffsetDateTime
import toml.Value
import scala.annotation.nowarn

class TomlDecodeException(
    val msg: String = "The toml does not match this data type."
) extends Exception

val TomlNode = toml.Value
type TomlNode = toml.Value

import scala.compiletime.{constValue, erasedValue}
import scala.quoted.*

inline def fetchNames[T <: Tuple]: List[String] =
  inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (x *: xs)  => constValue[x].asInstanceOf[String] :: fetchNames[xs]

def fetchTypes[T: Type](using q: Quotes): List[Type[?]] =
  Type.of[T] match
    case '[x *: xs]    => Type.of[x] :: fetchTypes[xs]
    case '[EmptyTuple] => Nil

trait TomlDecoder[T]:
  def decode(node: TomlNode): T

def fromToml[T: TomlDecoder](tomlString: String): T =
  val Right(node) = toml.Toml.parse(tomlString): @unchecked
  summon[TomlDecoder[T]].decode(node)

object TomlDecoder:
  inline def summonInstances[T <: Tuple]: List[TomlDecoder[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[TomlDecoder[t]] :: summonInstances[ts]

  given intDecoder: TomlDecoder[Int] with
    override def decode(node: TomlNode): Int =
      node match
        case TomlNode.Num(number) => number.intValue
        case _                    => throw new TomlDecodeException

  given longDecoder: TomlDecoder[Long] with
    override def decode(node: TomlNode): Long =
      node match
        case TomlNode.Num(number) => number.longValue
        case _                    => throw new TomlDecodeException

  given floatDecoder: TomlDecoder[Float] with
    override def decode(node: TomlNode): Float =
      node match
        case TomlNode.Real(number) => number.floatValue
        case _                     => throw new TomlDecodeException

  given doubleDecoder: TomlDecoder[Double] with
    override def decode(node: TomlNode): Double =
      node match
        case TomlNode.Real(number) => number.doubleValue
        case _                     => throw new TomlDecodeException

  given decimalDecoder: TomlDecoder[BigDecimal] with
    override def decode(node: TomlNode): BigDecimal =
      node match
        case TomlNode.Num(number) => BigDecimal(number.toString)
        case _                    => throw new TomlDecodeException

  given stringDecoder: TomlDecoder[String] with
    override def decode(node: TomlNode): String =
      node match
        case TomlNode.Str(string) => string
        case _                    => throw new TomlDecodeException

  given booleanDecoder: TomlDecoder[Boolean] with
    override def decode(node: TomlNode): Boolean =
      node match
        case TomlNode.Bool(boolean) => boolean
        case _                      => throw new TomlDecodeException

  given localDateDecoder: TomlDecoder[LocalDate] with
    override def decode(node: TomlNode): LocalDate =
      node match
        case TomlNode.Date(value) => value
        case _                    => throw new TomlDecodeException

  given localtimeDecoder: TomlDecoder[LocalTime] with
    override def decode(node: TomlNode): LocalTime =
      node match
        case TomlNode.Time(value) => value
        case _                    => throw new TomlDecodeException

  given localDateTimeDecoder: TomlDecoder[LocalDateTime] with
    override def decode(node: TomlNode): LocalDateTime =
      node match
        case TomlNode.DateTime(value) => value
        case _                        => throw new TomlDecodeException

  given offsetDateTimeDecoder: TomlDecoder[OffsetDateTime] with
    override def decode(node: TomlNode): OffsetDateTime =
      node match
        case TomlNode.OffsetDateTime(value) => value
        case _                              => throw new TomlDecodeException

  given listDecoder[T](using d: TomlDecoder[T]): TomlDecoder[List[T]] with
    override def decode(node: TomlNode): List[T] =
      node match
        case TomlNode.Arr(items) => items.map(i => d.decode(i))
        case _                   => throw new TomlDecodeException

  given optionDecoder[T](using d: TomlDecoder[T]): TomlDecoder[Option[T]] with
    override def decode(node: TomlNode): Option[T] =
      node match
        case null => None
        case x => Some(d.decode(x))

  @nowarn("id=92")
  private def newDecoderProduct[T](
      names: List[String],
      instances: List[TomlDecoder[?]],
      typedDefaultValues: Map[String, Any],
      metaData: TomlMetaData
  )(using m: Mirror.ProductOf[T]): TomlDecoder[T] =
    val info = names.zip(instances)

    val aliasNameMap = metaData.fieldNames
      .zip(metaData.aliasNames)
      .toMap
      .map((k, v) => k -> v.headOption.getOrElse(k))

    val defaultValueMap = metaData.fieldNames
      .zip(metaData.defaultValues)
      .toMap

    val ignoreMap = metaData.fieldNames
      .zip(metaData.ignore)
      .toMap

    new TomlDecoder[T]:
      override def decode(node: TomlNode): T =
        node match
          case TomlNode.Tbl(items) =>
            val data = info.map: (name, instance) =>
              val alias = aliasNameMap(name)
              (items.contains(alias), ignoreMap(name)) match
                case (true, _) =>
                  try instance.decode(items(alias))
                  catch
                    case e: TomlDecodeException =>
                      throw new TomlDecodeException(
                        s"The toml cannot be mapped to field '$name'."
                      )
                case (false, true) if defaultValueMap(name) ne None =>
                  defaultValueMap(name).get
                case (false, true) => typedDefaultValues(name)
                case (false, false) if instance.isInstanceOf[TomlDecoder[Option[?]]] => instance.decode(null)
                case (false, false) =>
                  throw new TomlDecodeException(
                    s"The toml does not contain a value for field '$name', consider adding an annotation @tomlIgnore or checking the toml."
                  )

            m.fromProduct(Tuple.fromArray(data.toArray))
          case _ => throw new TomlDecodeException

  private def newDecoderSum[T](
      names: List[String],
      instances: List[TomlDecoder[?]],
      metaData: List[TomlMetaData]
  ): TomlDecoder[T] =
    new TomlDecoder[T]:
      override def decode(node: TomlNode): T =
        node match
          case TomlNode.Str(string) =>
            if !names.contains(string) then throw new TomlDecodeException
            val index = names.indexOf(string)
            if metaData(index).fieldNames.isEmpty then
              instances(index)
                .asInstanceOf[TomlDecoder[T]]
                .decode(TomlNode.Tbl(Map()))
            else throw new TomlDecodeException
          case TomlNode.Tbl(items) =>
            val (name, node) = items.head
            if !names.contains(name) then throw new TomlDecodeException
            val index = names.indexOf(name)
            instances(index).asInstanceOf[TomlDecoder[T]].decode(node)
          case _ => throw new TomlDecodeException

  inline given derived[T](using m: Mirror.Of[T]): TomlDecoder[T] =
    val names = fetchNames[m.MirroredElemLabels]
    val instances = summonInstances[m.MirroredElemTypes]
    val metaData = fetchMetaData[m.MirroredElemTypes]

    inline m match
      case p: Mirror.ProductOf[T] =>
        val defaultValues = names
          .zip(DefaultValue.defaultValues[m.MirroredElemTypes])
          .toMap
        val metaData = tomlMetaDataMacro[T]
        newDecoderProduct[T](names, instances, defaultValues, metaData)(using p)
      case _: Mirror.SumOf[T] => newDecoderSum[T](names, instances, metaData)
