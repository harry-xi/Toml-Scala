package top.harryxi.toml

import scala.annotation.StaticAnnotation

@scala.annotation.meta.field
class tomlIgnore() extends StaticAnnotation

@scala.annotation.meta.field
case class tomlAlias(alias: String) extends StaticAnnotation