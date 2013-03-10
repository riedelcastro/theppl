package com.github.riedelcastro.theppl.logic

import com.github.riedelcastro.theppl.State
import javax.tools._
import java.net.URI
import javax.tools.JavaFileObject.Kind
import java.util
import collection.JavaConversions._
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import com.github.riedelcastro.theppl.IntVar
import javax.tools.JavaFileManager.Location

trait TestInterface {

}

/**
 * http://stackoverflow.com/questions/2946338/how-do-i-programmatically-compile-and-instantiate-a-java-class
 * @author Sebastian Riedel
 */
object TermCompiler {


  def compile[V](term: Term[V]): CompiledTerm[V] = {
    //create state data structure:
    //find free variables
    val variables = term.variables
    //we create a java class that has one member variable per variable term
    val packageName = "just.generated"
    val className = "State"
    val qualified = packageName + "." + className
    val classFile =
      """
        |package %s;
        |public class %s implements com.github.riedelcastro.theppl.logic.TestInterface {
        |    public %s() {}
        |%s
        |};
      """.stripMargin.format(packageName, className, className, "   public int test = 0;")
    val compiler = ToolProvider.getSystemJavaCompiler()
    val diagnosticsCollector = new DiagnosticCollector[JavaFileObject]()
    val fileManager = compiler.getStandardFileManager(diagnosticsCollector, null, null)
    val jfm = new RAMFileManager(fileManager)
    val javaFileAsString = SourceJavaFileObject(className + ".java",classFile)
    val task = compiler.getTask(null, jfm, diagnosticsCollector, null, null, util.Arrays.asList(javaFileAsString))
    val result = task.call()
    println(result)
    for (d <- diagnosticsCollector.getDiagnostics){
      println(d)
    }
    val clazz = Class.forName(qualified,false,jfm.loader)
    val instance = clazz.newInstance()
    println(instance)
//    val classLoader = URLClassLoader.newInstance(Array());



    //create one member variable per free variable
    //converter sets these member variables according to state
    //???
    null
  }

  def main(args: Array[String]) {
    val test = IntVar('test)
    val compiled = compile(test)
    println(compiled)
  }

}

class RAMFileManager(sjfm:StandardJavaFileManager)
  extends ForwardingJavaFileManager[StandardJavaFileManager](sjfm){

  var cache:Map[String, RAMJavaFileObject] = Map.empty

  val loader = new RAMClassLoader

  class RAMClassLoader extends ClassLoader {
    override def findClass(name: String) = {
      val result = cache.get(name).map(jfo => {
        val bytes = jfo.byteArrayOutStream.toByteArray
        defineClass(name,bytes,0,bytes.length)
      })
      result.getOrElse(super.findClass(name))
    }
  }

  override def getJavaFileForOutput(location: Location, name: String, kind: Kind, sibling: FileObject) = {
    val jfo = new RAMJavaFileObject(name,kind)
    cache += name -> jfo
    jfo
  }
  override def getClassLoader(loc: Location) = loader
  override def inferBinaryName(loc: Location, jfo: JavaFileObject) = {
    if (loc == StandardLocation.CLASS_PATH && jfo.isInstanceOf[RAMJavaFileObject]) jfo.getName
    else super.inferBinaryName(loc, jfo)
  }
  override def list(loc: Location, pkg: String, kind: util.Set[Kind], recurse: Boolean) = {
    val result = super.list(loc,pkg,kind,recurse).toSeq
    if (loc == StandardLocation.CLASS_PATH && pkg == "just.generated" && kind(JavaFileObject.Kind.CLASS)){
      result ++ cache.values
    } else
      result
  }
}


case class SourceJavaFileObject(name: String, content: String) extends SimpleJavaFileObject(new URI(name), Kind.SOURCE) {
  override def getCharContent(ignoreEncodingErrors: Boolean) = content
}


/**
 * A JavaFileObject that uses RAM instead of disk to store the file. It
 * gets written to by the compiler, and read from by the loader.
 */
class RAMJavaFileObject(name:URI, kind:Kind) extends SimpleJavaFileObject(name,kind) {

  private var baos:ByteArrayOutputStream  = null

  def this(name:String,kind:Kind) {
    this(new URI(name),kind)
  }

  def byteArrayOutStream = baos

  override def openInputStream() = {
    new ByteArrayInputStream(baos.toByteArray)
  }
  override def openOutputStream() = {
    baos = new ByteArrayOutputStream()
    baos
  }
  override def getCharContent(p1: Boolean) = {
    throw new UnsupportedOperationException();
  }
}

trait CompiledTerm[V] {

  def convertState(state: State): CompiledState
  def eval(state: CompiledState): V

}

trait CompiledState {

}
