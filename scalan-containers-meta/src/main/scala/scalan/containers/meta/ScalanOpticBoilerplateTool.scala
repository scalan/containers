package scalan.containers.meta

import scalan.meta.{EntityManagement, CodegenConfig, BoilerplateTool}

object ScalanContainersBoilerplateTool extends BoilerplateTool {
  lazy val scalanContainersConfig = CodegenConfig(
    name = "scalan-containers",
    srcPath = "scalan-containers-core/src/main/scala",
    entityFiles = List(
      "scalan/tables/Tables.scala"
      , "scalan/tables/Columns.scala"
      ),
    extraImports = List(
        "scala.reflect.runtime.universe._",
        "scalan.common.Default"),
    entityTypeSynonyms = Map()
  )

  override def getConfigs(args: Array[String]) = Seq(scalanContainersConfig)

  override def main(args: Array[String]) = {
    new EntityManagement(scalanContainersConfig).generateAll()
  }
}
