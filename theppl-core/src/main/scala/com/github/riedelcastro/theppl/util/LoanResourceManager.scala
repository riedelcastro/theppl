package com.github.riedelcastro.theppl.util

import java.io.{InputStream, UnsupportedEncodingException, IOException}
import scala.io.Source
import com.mongodb.{DBCollection, DB, Mongo}
import com.google.common.cache.{CacheBuilder, LoadingCache, CacheLoader}
import java.util.concurrent.TimeUnit
import org.riedelcastro.frontlets.{AbstractFrontlet, MongoFrontletCollection}

/**
 * Resource Manager according to the Loan Pattern.
 * https://wiki.scala-lang.org/display/SYGN/Loan
 *
 * Automatic file resource management, meaning proper close of all open resources.
 * Execution logic on the open resource is passed by the application.
 *
 * Created by larysa  04.11.13
 */
object LoanResourceManager {

  /**
   * Opens MongoDB connection and perform provided operations
   * on it, after that the connection will be closed.
   * @param dbName connect to this particular database.
   * @param process passed function to be invoked on an open connection
   */
  def withMongoConnector(dbName: String)(process: DB => Unit) {
    val config: String = scala.util.Properties.propOrElse("config", "config.properties")
    val properties = new java.util.Properties()
    properties.load(Util.getStreamFromFileOrClassPath(config))
    val host = properties.getProperty("mongo.host", "localhost")
    val port = properties.get("mongo.port", 27017)

    val mongoConn = new Mongo(host)
    val mongoDB: DB = mongoConn.getDB(dbName)

    process(mongoDB)

    mongoConn.close()
  }

  /* Creates an InputStream from file and applies to each line the provided function.  */
  def withFileIterator(file: String)(process: String => Unit) {
    val stream = Util.getStreamFromClassPathOrFile(file)
    try {
      val lines: Iterator[String] = Source.fromInputStream(stream).getLines()
      lines foreach (line => process(line))
    }
    catch {
      case e: IOException => throw e
      case e: Exception => throw e
    } finally {
      if (stream != null) {
        try {
          stream.close()
        } catch {
          case e: Exception => throw e
        }
      }
    }
  }


  /* File resource management, tailored for an (Alchemy) MLN file*/
  def withFileIteratorForMLN(file: String)(process: String => Unit) {
    val stream: InputStream = Util.getStreamFromClassPathOrFile(file)
    try {
      val lines = Source.fromInputStream(stream).getLines().filter(nonMLNElements(_))
      lines foreach (line => process(line))
    } catch {
      case e: IOException => throw e
      case e: Exception => throw e
    } finally {
      if (stream != null) {
        try {
          stream.close()
        } catch {
          case e: Exception => throw e
        }
      }
    }
  }

  private def nonMLNElements(x: String): Boolean = {
    !((x startsWith "//") || (x isEmpty))
  }

}

/**
 * Factory object to create a connection to a MongoDB instance.
 * Custom connection data can be placed in the config.properties file, which
 * assumed to be in the classpath or the path string is provided as -Dconfig parameter.
 * Default values for the connection string are as usual:
 * "mongo.host", "localhost"
 * "mongo.port", "27017"
 * Note that the same properties are expected to be in the config.properties file.
 */
object MongoFactory {

  private val config: String = scala.util.Properties.propOrElse("config", "config.properties")
  private val properties = new java.util.Properties()
  properties.load(Util.getStreamFromFileOrClassPath(config))

  private val HOST: String = properties.getProperty("mongo.host", "localhost")
  private val PORT: Int = properties.getProperty("mongo.port", "27017").toInt

  val connection = new Mongo(HOST, PORT)

  def forDB(name: String): DB = connection.getDB(name)
}

/**
 * Cache for <code>MongoFrontletCollection</code> objects.
 *
 */
object FrontletsMongoCache {

  private def frontletsLoader[C <: AbstractFrontlet](dbName: String, constr: () => C): CacheLoader[String, MongoFrontletCollection[C]] =
    new CacheLoader[String, MongoFrontletCollection[C]]() {
      def load(p1: String): MongoFrontletCollection[C] = {
        val coll: DBCollection = MongoFactory.forDB(dbName).getCollection(p1)
        new MongoFrontletCollection(coll, constr)
      }
    }

  def from[C <: AbstractFrontlet](dbName: String, constr: () => C): LoadingCache[String, MongoFrontletCollection[C]] =
    CacheBuilder.newBuilder()
      .maximumSize(100)
      .expireAfterWrite(10, TimeUnit.MINUTES)
      .build(frontletsLoader(dbName, constr))
}

object MongoDBCache {

  private def collLoader(dbName: String): CacheLoader[String, DBCollection] = new CacheLoader[String, DBCollection] {
    def load(key: String): DBCollection = {
      MongoFactory.forDB(dbName).getCollection(key)
    }
  }

  def from(dbName: String): LoadingCache[String, DBCollection] =
    CacheBuilder.newBuilder()
      .maximumSize(100)
      .expireAfterWrite(10, TimeUnit.MINUTES)
      .build(collLoader(dbName))

}




