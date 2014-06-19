package edu.umass.cs.iesl.pdf2meta.cli.lib


import com.typesafe.config.{ConfigRenderOptions, ConfigFactory, Config}
import com.typesafe.config.ConfigValue


/**
 * TODO: make a lib of this class, since it is also used in pdf2meta-web
 * * Created by klimzaporojets on 6/17/14.
 * Manage properties file, read and write to .properties
 */
class MapToProperties {

  def readPropertiesFile(fileName:String):Map[String,String] =
  {
    val conf:Config = ConfigFactory.parseFile(new java.io.File(fileName))
    val convSet = scala.collection.JavaConverters.asScalaSetConverter(conf.entrySet()).asScala

    val testres = convSet.map(x=> (Map(x.getKey -> x.getValue)))
    def convertToMap(setOfMap:scala.collection.mutable.Set[Map[String,ConfigValue]]):Map[String,String] =
    {
      if(setOfMap.size>1)
      {
        val nextValue = setOfMap.head.valuesIterator.next().render()
        Map(setOfMap.head.keysIterator.next() -> nextValue.substring(1, nextValue.length-1) ) ++ convertToMap(setOfMap.tail)
      }
      else
      {
        //        val nextValue = setOfMap.head.valuesIterator.next().render()
        if(setOfMap.size>0) {
          val nextValue = setOfMap.head.valuesIterator.next().render()
          Map(setOfMap.head.keysIterator.next() -> nextValue.substring(1, nextValue.length - 1))
        }
        else
        {
          Map()
        }
      }
    }
    val testres2 = convertToMap(testres)

    testres2

  }

  def savePropertiesValues(fileName:String, properties:Map[String,String])
  {
    val mapped = scala.collection.JavaConversions.mapAsJavaMap(properties)
    val c:Config = ConfigFactory.parseMap(mapped)
    println(c.root().render(ConfigRenderOptions.defaults().setJson(false).setComments(false).setFormatted(false).setOriginComments(false)))
    scala.tools.nsc.io.File(fileName).writeAll(c.root().render(ConfigRenderOptions.defaults().setJson(false).setComments(false).setOriginComments(false)).replaceAll("=\"","=").replaceAll("\"\n","\n") )
  }

  def addToProperties(fileName:String,properties:Map[String,String])
  {
    val toWrite:Map[String,String] = readPropertiesFile(fileName) ++ properties
    savePropertiesValues(fileName, toWrite)
  }

  def addOrReplaceValue(fileName:String,property:String,value:String)
  {
    val allProperties:Map[String,String] = readPropertiesFile(fileName)

    val filteredProperty = {if(allProperties.get(property)!=None){
      allProperties - property
    }else{allProperties}}

    val entry = Map(property -> value)

    savePropertiesValues(fileName,filteredProperty ++ entry)
  }

}

