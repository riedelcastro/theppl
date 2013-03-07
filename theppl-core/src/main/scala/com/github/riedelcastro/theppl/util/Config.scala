package com.github.riedelcastro.theppl.util

import java.util.Properties
import java.io.{File, FileInputStream, InputStream}

/**
 * Very simple configuration file loader based on java properties files.
 * @author sriedel
 */
class Config(val properties: Properties) {
  def this(in: InputStream) {
    this ({val props = new Properties; props.load(in); props})
  }

  def get[T](key: String)(implicit m: Manifest[T]): T = {
    val value = properties.getProperty(key)
    if (value == null) sys.error("No value associated with key " + key)
    createBySimpleErasureName(m.runtimeClass.getSimpleName, value)
  }

  def get[T](key: String, default: T)(implicit m: Manifest[T]): T = {
    val value = properties.getProperty(key)
    if (value == null) default else createBySimpleErasureName(m.runtimeClass.getSimpleName, value)
  }

  private def createBySimpleErasureName[T](name: String, untrimmed: String): T = {
    val value = untrimmed.trim
    val result = name match {
      case "int" => value.toInt
      case "String" => value
      case "double" => value.toDouble
      case "boolean" => value.toBoolean
      case "File" => new File(value)
      case "FileInputStream" => new FileInputStream(value)
      case x if (x.endsWith("[]")) => {
        val prefix = x.substring(0, x.indexOf('['))
        val split = value.split(",").map(_.trim).map(createBySimpleErasureName(prefix, _))
        split
      }
      case x => sys.error("Can't convert type " + x)
    }
    result.asInstanceOf[T]
  }

  def put[T](key: String, value: T) = {
    properties.put(key, value.asInstanceOf[Object])
  }

}

/**
 * Loads user and environment specific property files.
 */
object Config extends HasLogger {

  import java.net._

  def getProjectConfStream(project:String) = getConfStream( project + "-" + userName() + "@" + short())

  def getConfStream(name: String = "stratifie-" + userName() + "@" + short()): InputStream = {
    if (!name.contains("-"))
      Util.getStreamFromFileOrClassPath(name + ".properties")
    else try {
      Util.getStreamFromFileOrClassPath(name + ".properties")
    } catch {
      case e:Throwable =>
        logger.info("Cannot find %s.properties, backing-off".format(name))
        //try with username

        try {
          getConfStream(name.substring(0, name.lastIndexOf("-")))
        } catch {
          case e1:Throwable =>
            //try without username
            if (name.contains("@")) {
              val atIndex = name.lastIndexOf("@")
              val firstDashIndex = name.indexOf("-")
              val removed = name.take(firstDashIndex+1) ++ name.substring(atIndex + 1)
              getConfStream(removed)
            } else throw e1
        }


      //try without username

    }
  }

  def userName() = {
    System.getProperty("user.name");
  }

  def long() = {
    try {
      InetAddress.getLocalHost.getHostName
    } catch {
      case _:Throwable => "unknown"
    }
  }

  def short() = {
    val result = try {
      InetAddress.getLocalHost.getHostName.toLowerCase
    } catch {
      case _:Throwable => "unknown"
    }
    val lastDot = result.indexOf(".")
    if (lastDot < 0) result else result.substring(0, lastDot)
  }

}

