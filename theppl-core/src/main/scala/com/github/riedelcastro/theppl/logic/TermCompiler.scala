package com.github.riedelcastro.theppl.logic

import com.github.riedelcastro.theppl.{Variable, State, IntVar}
import javax.tools._
import java.net.URI
import javax.tools.JavaFileObject.Kind
import java.util
import collection.JavaConversions._
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import javax.tools.JavaFileManager.Location
import collection.mutable

trait CompiledState {
  def setState(state: State)

}

/**
 * http://stackoverflow.com/questions/2946338/how-do-i-programmatically-compile-and-instantiate-a-java-class
 * @author Sebastian Riedel
 */
object TermCompiler {

  val packageName = "just.generated"

  def indent(count: Int)(text: String) = Array.fill(count)(" ").mkString + text

  class CompilationContext[V](val term: Term[V]) {
    val cache = new VarNameCache
    val variables = term.variables.toSeq
    val index2variable = variables.zipWithIndex.toMap
  }

  class VarNameCreator {
    val index = 0
    val counts = new mutable.HashMap[String, Int]()
    def createName(v: Variable[Any]) = {
      val count = counts.getOrElse(v.stringRepr, 0)
      val name = v.stringRepr + count
      counts(v.stringRepr) = count + 1
      name
    }
  }

  class VarNameCache {
    val creator = new VarNameCreator
    val cache = new mutable.HashMap[Variable[Any], String]()
    def getName(variable: Variable[Any]) = {
      cache.getOrElseUpdate(variable, creator.createName(variable))
    }
  }


  def createCompiledStateSource[V](className: String)(implicit context: CompilationContext[V]) = {
    import context._
    //create state data structure:
    //find free variables
    //we create a java class that has one member variable per variable term
    val members = for (v <- variables) yield {
      val name = cache.getName(v)
      val helper = javaTypeHelper(v)
      indent(4) { "public %s %s = %s;".format(helper.typ, name, v.default) }
    }

    //member initialization
    val inits = for (v <- variables) yield {
      val helper = javaTypeHelper(v)
      val varIndex = index2variable(v)
      val option = indent(12) { "Option tmp = state.get(variables.apply(%d));".format(varIndex) }
      val ifDefined = indent(12) { "if (tmp.isDefined()) %s = %s;".format(
        cache.getName(v),
        helper.unbox("((%s)tmp.get())".format(helper.boxedType))) }
      Seq(option,ifDefined).mkString("\n")
    }

    println(inits.mkString("\n"))

    val stateSource =
      """
        |package %s;
        |
        |import com.github.riedelcastro.theppl.*;
        |import com.github.riedelcastro.theppl.logic.CompiledState;
        |import scala.collection.*;
        |import scala.Option;
        |
        |public class %s implements CompiledState {
        |
        |    Seq<Variable<Object>> variables;
        |
        |    public %s(Seq<Variable<Object>> variables) {
        |        this.variables = variables;
        |    }
        |
        |%s
        |
        |    public void setState(State state) {
        |%s
        |    }
        |
        |};
      """.stripMargin.format(packageName, className, className, members.mkString("\n"),inits.mkString("\n"))
    stateSource

  }

  trait JavaTypeHelper[+V] {
    def typ:String
    def boxedType:String = typ
    def unbox(expression:String):String
  }

  def javaTypeHelper(v:Variable[Any]):JavaTypeHelper[Any] = {
    v.default match {
      case i: Int => new JavaTypeHelper[Int] {
        def typ = "int"
        override def boxedType = "Integer"
        def unbox(expression: String) = expression + ".intValue()"
      }
      case x=> new JavaTypeHelper[Any] {
        def typ = "Object"
        def unbox(expression: String) = expression
      }

    }
  }

  def compile[V](term: Term[V]): CompiledTerm[V] = {
    val className = "CompiledStateForTerm"
    val qualified = packageName + "." + className
    implicit val context = new CompilationContext(term)
    import context._
    val stateSource = createCompiledStateSource(className)
    println(stateSource)
    val compiler = ToolProvider.getSystemJavaCompiler()
    val diagnosticsCollector = new DiagnosticCollector[JavaFileObject]()
    val fileManager = compiler.getStandardFileManager(diagnosticsCollector, null, null)
    val jfm = new RAMFileManager(fileManager)
    val javaFileAsString = SourceJavaFileObject(className + ".java", stateSource)
    val task = compiler.getTask(null, jfm, diagnosticsCollector, null, null, util.Arrays.asList(javaFileAsString))
    val result = task.call()
    println(result)
    for (d <- diagnosticsCollector.getDiagnostics) {
      println(d)
    }
    val clazz = Class.forName(qualified, false, jfm.loader)
    val state = State(Map.empty)
    val instance = clazz.getDeclaredConstructor(classOf[Seq[Variable[Any]]]).newInstance(context.term.variables.toSeq)
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

class RAMFileManager(sjfm: StandardJavaFileManager)
  extends ForwardingJavaFileManager[StandardJavaFileManager](sjfm) {

  var cache: Map[String, RAMJavaFileObject] = Map.empty

  val loader = new RAMClassLoader

  class RAMClassLoader extends ClassLoader {
    override def findClass(name: String) = {
      val result = cache.get(name).map(jfo => {
        val bytes = jfo.byteArrayOutStream.toByteArray
        defineClass(name, bytes, 0, bytes.length)
      })
      result.getOrElse(super.findClass(name))
    }
  }

  override def getJavaFileForOutput(location: Location, name: String, kind: Kind, sibling: FileObject) = {
    val jfo = new RAMJavaFileObject(name, kind)
    cache += name -> jfo
    jfo
  }
  override def getClassLoader(loc: Location) = loader
  override def inferBinaryName(loc: Location, jfo: JavaFileObject) = {
    if (loc == StandardLocation.CLASS_PATH && jfo.isInstanceOf[RAMJavaFileObject]) jfo.getName
    else super.inferBinaryName(loc, jfo)
  }
  override def list(loc: Location, pkg: String, kind: util.Set[Kind], recurse: Boolean) = {
    val result = super.list(loc, pkg, kind, recurse).toSeq
    if (loc == StandardLocation.CLASS_PATH && pkg == TermCompiler.packageName && kind(JavaFileObject.Kind.CLASS)) {
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
class RAMJavaFileObject(name: URI, kind: Kind) extends SimpleJavaFileObject(name, kind) {

  private var baos: ByteArrayOutputStream = null

  def this(name: String, kind: Kind) {
    this(new URI(name), kind)
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

