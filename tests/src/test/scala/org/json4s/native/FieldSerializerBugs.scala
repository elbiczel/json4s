package org.json4s

import org.specs2.mutable.Specification

object FieldSerializerBugs extends Specification {
  import native.JsonMethods._
  import native.Serialization
  import Serialization.{read, write => swrite}

  implicit val formats = DefaultFormats + FieldSerializer[AnyRef]()

/* FIXME: it doesn't cause a stack overflow but the ser/deser doesn't work
  "AtomicInteger should not cause stack overflow" in {
    import java.util.concurrent.atomic.AtomicInteger

    val ser = swrite(new AtomicInteger(1))
    val atomic = read[AtomicInteger](ser)
    atomic.get must_== 1
  }
*/

  "Serializing a singleton object should not cause stack overflow" in {
    swrite(SingletonObject) must not(throwAn[Exception])
  }

  "Name with symbols is correctly serialized" in {
    implicit val formats = DefaultFormats + FieldSerializer[AnyRef]()

    val s = WithSymbol(5)
    val str = Serialization.write(s)
    str must_== """{"a-b*c":5}"""
    read[WithSymbol](str) must_== s
  }

  "FieldSerialization should work with Options" in {
    implicit val formats = DefaultFormats + FieldSerializer[ClassWithOption]()

    val t = new ClassWithOption
    t.field = Some(5)
    read[ClassWithOption](Serialization.write(t)).field must_== Some(5)
  }

  "FieldSerializer's manifest should not be overridden when it's added to Formats" in {
    val fieldSerializer = FieldSerializer[Type1](FieldSerializer.renameTo("num", "yum"))
    implicit val formats = DefaultFormats + (fieldSerializer: FieldSerializer[_])
    val expected1 = JObject(JField("yum", JInt(123)))
    val expected2 = JObject(JField("num", JInt(456)))
    Extraction.decompose(Type1(123)) must_== (expected1)
    Extraction.decompose(Type2(456)) must_== (expected2)
  }

  "FieldSerializer's must work correctly for registered superinterface" in {
    // generic field serializer
    val genericFieldSerializer = FieldSerializer[SuperTrait](FieldSerializer.ignore("ignoredField"))
    // specific field serializer
    val specificFieldSerializer = FieldSerializer[MainTrait](
      FieldSerializer.ignore("ignoredField") orElse FieldSerializer.renameTo("importantField", "important"))
    implicit val formats = DefaultFormats + specificFieldSerializer + genericFieldSerializer
    val expected1 = JObject(JField("important", JString("importantValue")))
    val mainObj = new MainClass("ignoredValue", "importantValue")
    val decomposed = Extraction.decompose(mainObj)
    decomposed must_== (expected1)
  }

  "FieldSerializer's must take the best serializer" in {
    // trait serializer
    val specificFieldSerializer = FieldSerializer[MainTrait](
      FieldSerializer.ignore("ignoredField") orElse FieldSerializer.renameTo("importantField", "important"))
    // class serializer
    val classFieldSerializer = FieldSerializer[MainClass](
      FieldSerializer.ignore("ignoredField") orElse FieldSerializer.renameTo("importantField", "importantClassField"))
    implicit val formats = DefaultFormats + classFieldSerializer + specificFieldSerializer
    val expected1 = JObject(JField("importantClassField", JString("importantValue")))
    val mainObj = new MainClass("ignoredValue", "importantValue")
    val decomposed = Extraction.decompose(mainObj)
    decomposed must_== (expected1)
  }

  trait SuperTrait

  trait MainTrait extends SuperTrait {
    val ignoredField: String
    val importantField: String
  }

  class MainClass(override val ignoredField: String, override val importantField: String) extends MainTrait

  case class WithSymbol(`a-b*c`: Int)

  class ClassWithOption {
    var field: Option[Int] = None
  }

  case class Type1(num: Int)
  case class Type2(num: Int)

  object SingletonObject
}


